library(lme4)
library(lmerTest)
library(dplyr)
library(stringr)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/10M")

file_list <- c(
  "results_experiment_1_OPT_10M.csv",
  "results_experiment_1_gpt_bert_10M_causal.csv",
  "results_experiment_1_babyLlama_10M.csv",
  "results_experiment_1_gpt_2_10M.csv",
  "results_experiment_1_distill_tree__95_10M.csv",
  "results_experiment_1_ZLATA.csv",
  "results_experiment_1_gpt_bert_10M_masked.csv",
  "results_experiment_1_gpt_bert_10M_mixed.csv",
  "results_experiment_1_what_if_baseline.csv",
  "results_experiment_1_MOEP.csv"
  
)

get_model_name <- function(fn){
  x <- fn
  x <- str_replace(x, "\\.csv$", "")
  x <- str_replace(x, "^results_experiment_\\d+_", "")
  x
}

dat <- lapply(file_list, function(f){
  d <- read.csv(f, check.names = TRUE)
  d$model <- get_model_name(f)
  d
}) |> bind_rows()

dat <- dat %>%
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular", "Irregular")),
    plurality  = factor(plurality, levels = c("Singular", "Plural")),
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
  ) %>%
  filter(!is.na(set))

m_interaction <- lmer(
  Surprisal.head ~ 1 + regularity * plurality + (1 + plurality | model) + (1 + regularity_num || set) + (1 + regularity_num * plurality_num || Head),
  data = dat,
  REML = TRUE
)

m_nested <- lmer(
  Surprisal.head ~ 1 + regularity / plurality + (1 + plurality | model) + (1 + regularity_num || set) + (1 + regularity_num * plurality_num || Head),
  data = dat,
  REML = TRUE
)

cat("\n--- INTERACTION ---\n")
cat("AIC:", AIC(m_interaction), "\n")
cat("Singular:", isSingular(m_interaction), "\n")
print(summary(m_interaction))

cat("\n--- NESTED ---\n")
cat("AIC:", AIC(m_nested), "\n")
cat("Singular:", isSingular(m_nested), "\n")
print(summary(m_nested))