library(brms)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggeffects)

options(scipen = 9999)
options(digits = 3)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/10M")

file_list <- list.files(pattern="\\.csv$", full.names = FALSE)

# function to convert the file name into the model name
get_model_name <- function(fn){
  x <- fn
  x <- str_replace(x, "\\.csv$", "")
  x <- str_replace(x, "^results_experiment_\\d+_", "")
  x
}

# -------------------------------------------------------------------------------------------
### Load + stack all CSVs

dat <- lapply(file_list, function(f){
  d <- read.csv(f, check.names = TRUE)
  d$model <- get_model_name(f)
  d$File <- f
  d
}) |> bind_rows()

# -------------------------------------------------------------------------------------------
### Data preparation (treatment; Plural reference)

dat <- dat %>%
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular", "Irregular")),
    plurality  = factor(plurality,  levels = c("Plural", "Singular")),
    model      = factor(model)
  )

dat$regularity_num <- ifelse(dat$regularity == "Regular", 0, 1)
dat$plurality_num  <- ifelse(dat$plurality  == "Plural", 0, 1)

dat <- dat %>%
  mutate(
    set = case_when(
      Non.Head %in% c("goose","geese","swan","swans") ~ "set_goose_swan",
      Non.Head %in% c("ox","oxen","cow","cows") ~ "set_ox_cow",
      Non.Head %in% c("louse","lice","flea","fleas") ~ "set_louse_flea",
      Non.Head %in% c("mouse","mice","rat","rats") ~ "set_mouse_rat",
      Non.Head %in% c("foot","feet","leg","legs") ~ "set_foot_leg",
      Non.Head %in% c("tooth","teeth","bone","bones") ~ "set_tooth_bone",
      Non.Head %in% c("child","children","adult","adults") ~ "set_child_adult",
      Non.Head %in% c("woman","women","girl","girls") ~ "set_woman_girl",
      Non.Head %in% c("man","men","boy","boys") ~ "set_man_boy",
      Non.Head %in% c("salesman","salesmen","retailer","retailers") ~ "set_salesman_retailer",
      Non.Head %in% c("nobleman","noblemen","aristocrat","aristocrats") ~ "set_nobleman_aristocrat",
      Non.Head %in% c("boatman","boatmen","shipmate","shipmates") ~ "set_boatman_shipmate",
      Non.Head %in% c("craftsman","craftsmen","labourer","labourers") ~ "set_craftsman_labourer",
      Non.Head %in% c("fireman","firemen","lifeguard","lifeguards") ~ "set_fireman_lifeguard",
      TRUE ~ NA_character_
    )
  )

# drop NA sets
dat <- dat %>% filter(!is.na(set))

# -------------------------------------------------------------------------------------------
### PUT THE SELECTED FORMULA HERE

formula_cross <- bf(
  Surprisal.head ~ 1 + regularity * plurality + (1 + regularity * plurality | model)
)

# -------------------------------------------------------------------------------------------
### FIT BAYESIAN MODEL (NO EXPLICIT PRIORS YET)

m_cross <- brm(formula_cross,
               data = dat,
               family = gaussian())

# -------------------------------------------------------------------------------------------
### BASIC OUTPUT

summary(m_cross)

prior_summary(m_cross) |> print(show_df = FALSE)

fixef(m_cross)

summary(m_cross)$random$model["Estimate"]
summary(m_cross)$random$set["Estimate"]
summary(m_cross)$random$Head["Estimate"]
# -------------------------------------------------------------------------------------------
### POSTERIOR PREDICTIVE CHECK

pp_check(m_cross)

# -------------------------------------------------------------------------------------------
### MODEL-BASED PREDICTED MEANS

m_preds <- ggeffects::ggemmeans(m_cross, c("plurality", "regularity"))
m_preds <- as.data.frame(m_preds)

# -------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------
### PLOT

y_pad <- 0.2 * diff(range(c(m_preds$conf.low, m_preds$conf.high)))

p <- ggplot(
  m_preds,
  aes(x = x, y = predicted, group = group, colour = group)
) +
  geom_point(size = 3, position = position_dodge(width = 0.18)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    linewidth = 0.8,
    position = position_dodge(width = 0.18)
  ) +
  coord_cartesian(
    ylim = c(min(m_preds$conf.low) - y_pad, max(m_preds$conf.high) + y_pad)
  ) +
  labs(
    x = "Plurality",
    y = "Surprisal of head noun",
    colour = "Regularity"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 14, colour = "gray28"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

print(p)

ggsave(
  "bayesian_cross_model_ggemmeans_plot.png",
  plot = p,
  width = 8,
  height = 7,
  dpi = 300
)

#___________________________________

# -------------------------------------------------------------------------------------------
### CROSS-MODEL FOREST PLOT BY MODEL
### Values derived from the single cross-model Bayesian model

coef_model <- coef(m_cross, summary = FALSE)$model

model_levels <- dimnames(coef_model)[[2]]
coef_names   <- dimnames(coef_model)[[3]]

n_draws  <- dim(coef_model)[1]
n_models <- dim(coef_model)[2]

zero_mat <- matrix(0, nrow = n_draws, ncol = n_models)

if ("pluralitySingular" %in% coef_names) {
  beta_p <- coef_model[, , "pluralitySingular"]
} else {
  beta_p <- zero_mat
}

if ("regularityIrregular:pluralitySingular" %in% coef_names) {
  beta_i <- coef_model[, , "regularityIrregular:pluralitySingular"]
} else {
  beta_i <- zero_mat
}

# Treatment-coding interpretation:
# Regular plural effect    = -(pluralitySingular)
# Irregular plural effect  = -(pluralitySingular + interaction)
# Interaction              = regularityIrregular:pluralitySingular

draws_regular    <- -beta_p
draws_irregular  <- -(beta_p + beta_i)
draws_interaction <- beta_i

summarise_effect <- function(draw_matrix, effect_label){
  data.frame(
    model = model_levels,
    effect = effect_label,
    estimate = apply(draw_matrix, 2, mean),
    lower95  = apply(draw_matrix, 2, quantile, probs = 0.025),
    upper95  = apply(draw_matrix, 2, quantile, probs = 0.975),
    stringsAsFactors = FALSE
  )
}

plot_dat <- bind_rows(
  summarise_effect(draws_interaction,
                   "Regularity × plurality interaction"),
  summarise_effect(draws_regular,
                   "Regular plural effect\n(e.g., ducks trader vs. duck trader)"),
  summarise_effect(draws_irregular,
                   "Irregular plural effect\n(e.g., geese trader vs. goose trader)")
)

plot_dat$effect <- factor(
  plot_dat$effect,
  levels = c(
    "Regularity × plurality interaction",
    "Regular plural effect\n(e.g., ducks trader vs. duck trader)",
    "Irregular plural effect\n(e.g., geese trader vs. goose trader)"
  )
)

interaction_order <- plot_dat %>%
  filter(effect == "Regularity × plurality interaction") %>%
  arrange(estimate) %>%
  pull(model)

plot_dat$model <- factor(plot_dat$model, levels = interaction_order)

write.csv(plot_dat, "bayesian_cross_model_forest_values.csv", row.names = FALSE)

p_cross_forest <- ggplot(plot_dat, aes(x = model, y = estimate)) +
  coord_flip() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue") +
  facet_wrap(~ effect, ncol = 3) +
  labs(
    x = "Model",
    y = "Estimate"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 12, colour = "gray28"),
    strip.text = element_text(size = 11),
    axis.text.y = element_text(size = 13)
  )

print(p_cross_forest)

ggsave(
  "bayesian_cross_model_forest_plot.png",
  plot = p_cross_forest,
  width = 16,
  height = 7,
  dpi = 300
)