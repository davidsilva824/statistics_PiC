library(lme4)
library(lmerTest)
library(dplyr)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1")

file <- "results_experiment_1_babyLlama_100M.csv"
dat0 <- read.csv(file, check.names = TRUE)

make_set <- function(df){
  df %>%
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
}

# -------------------------------
# Plural as reference (treatment)
# -------------------------------
dat_plur <- dat0 %>%
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular","Irregular")),
    plurality  = factor(plurality,  levels = c("Plural","Singular")),
    regularity_num = ifelse(regularity == "Regular", 0, 1),
    plurality_num  = ifelse(plurality  == "Plural", 0, 1)
  ) %>%
  make_set()

final_model_plurref <- lmer(
  Surprisal.head ~ 1 + regularity * plurality +
    (1 + regularity_num ||      set) + (1 + plurality | Head),
  data = dat_plur,
  REML = TRUE
)

cat("\n--- Plural reference ---\n")
print(summary(final_model_plurref))
cat("isSingular:", isSingular(final_model_plurref), "\n")
cat("isSingular tol=1e-6:", isSingular(final_model_plurref, tol = 1e-6), "\n")

# ---------------------------------
# Singular as reference (treatment)
# ---------------------------------
dat_sing <- dat0 %>%
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular","Irregular")),
    plurality  = factor(plurality,  levels = c("Singular","Plural")),
    regularity_num = ifelse(regularity == "Regular", 0, 1),
    plurality_num  = ifelse(plurality  == "Singular", 0, 1)
  ) %>%
  make_set()

final_model_singref <- lmer(
  Surprisal.head ~ 1 + regularity * plurality +
    (1 + regularity_num ||      set) + (1 + plurality | Head),
  data = dat_sing,
  REML = TRUE
)

cat("\n--- Singular reference ---\n")
print(summary(final_model_singref))
cat("isSingular:", isSingular(final_model_singref), "\n")
cat("isSingular tol=1e-6:", isSingular(final_model_singref, tol = 1e-6), "\n")

