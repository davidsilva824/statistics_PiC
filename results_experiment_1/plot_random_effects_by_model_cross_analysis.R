library(lme4)
library(lmerTest)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)

# this folder contains the results files obtained from study_pic_4: https://github.com/davidsilva824/study_pic_4 
setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/10M")

# Files used in the cross-model analysis
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
### Data preparation (centered contrasts)

dat <- dat %>%
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular", "Irregular")),
    plurality  = factor(plurality,  levels = c("Singular", "Plural")),
    model      = factor(model)
  )

contrasts(dat$regularity) <- c(-0.5, 0.5)
contrasts(dat$plurality)  <- c(-0.5, 0.5)

dat$regularity_num <- ifelse(dat$regularity == "Regular", -0.5, 0.5)
dat$plurality_num  <- ifelse(dat$plurality  == "Singular", -0.5, 0.5)

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
### Fit the selected joint model

m_joint <- lmer(
  Surprisal.head ~ 1 + regularity * plurality +
    (1 + plurality | model) +
    ((1 | set) + (0 + regularity_num | set) + (0 + plurality_num | set) + (0 + regularity_num:plurality_num | set)) +
    ((1 | Head) + (0 + plurality_num | Head)),
  data = dat,
  REML = TRUE
)

# -------------------------------------------------------------------------------------------
### Extract random effects for model

random.effects <- as.data.frame(ranef(m_joint, condVar = TRUE))

random.effects <- random.effects %>%
  mutate(
    lower95 = condval - 1.96 * condsd,
    upper95 = condval + 1.96 * condsd
  )

random.model <- random.effects %>%
  filter(grpvar == "model") %>%
  droplevels() %>%
  mutate(
    grp = fct_reorder2(grp, term, condval, .desc = FALSE)
  )

facet_names <- c(
  "(Intercept)" = "Intercept",
  "plurality1"  = "Plurality slope"
)

# -------------------------------------------------------------------------------------------
### Plot

p <- ggplot(random.model, aes(x = grp, y = condval)) +
  coord_flip() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue") +
  facet_wrap(term ~ ., ncol = 1, scales = "free_y", labeller = as_labeller(facet_names)) +
  labs(
    x = "Model",
    y = "Random-effects adjustments",
    title = "Random effects by model"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 14, colour = "gray28"),
    strip.text = element_text(size = 16)
  )

print(p)

ggsave(
  filename = "forest_plot_random_effects_model.png",
  plot = p,
  width = 9,
  height = 8,
  dpi = 300
)