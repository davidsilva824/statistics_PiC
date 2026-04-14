library(lme4)
library(lmerTest)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggeffects)

# this folder contains the results files obtained from study_pic_4
setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/10M")

file_list <- list.files(pattern = "\\.csv$", full.names = FALSE)

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
  ) %>%
  filter(!is.na(set))

# -------------------------------------------------------------------------------------------
### PUT THE SELECTED FORMULA HERE

selected_formula <- Surprisal.head ~ 1 + regularity * plurality + (1 + plurality | model) + (1 + regularity_num || set) + (1 + regularity_num * plurality_num || Head)

# -------------------------------------------------------------------------------------------
### Fit model

m_cross <- lmer(selected_formula, data = dat, REML = TRUE)

# -------------------------------------------------------------------------------------------
### Get model-based predicted means

m_preds <- ggeffects::ggemmeans(m_cross, c("plurality", "regularity"))
m_preds <- as.data.frame(m_preds)

# -------------------------------------------------------------------------------------------
### Plot

pd <- position_dodge(width = 0.18)

y_pad <- 0.2 * diff(range(c(m_preds$conf.low, m_preds$conf.high)))

p <- ggplot(
  m_preds,
  aes(x = x, y = predicted, colour = group)
) +
  geom_point(size = 3, position = pd) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    linewidth = 0.8,
    position = pd
  ) +
  coord_cartesian(
    ylim = c(min(m_preds$conf.low) - y_pad, max(m_preds$conf.high) + y_pad)
  ) +
  labs(
    x = "Plurality",
    y = "Surprisal",
    colour = "Regularity"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 14, colour = "gray28"),
    axis.title = element_text(size = 19),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 19),
    legend.text = element_text(size = 18)
  )

print(p)

write.csv(m_preds, "ggemmeans_values.csv", row.names = FALSE)

ggsave(
  "ggemmeans_plot.png",
  plot = p,
  width = 8,
  height = 7,
  dpi = 300
)