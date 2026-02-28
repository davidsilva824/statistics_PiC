library(lme4)
library(lmerTest)
library(dplyr)
library(stringr)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1")

file_list <- c(
  "results_experiment_1_babyLlama_100M.csv",
  "results_experiment_1_babylm__opt-125m-strict-2023.csv",
  "results_experiment_1_BabyLM-community__babylm-baseline-100m-gpt-bert-causal-focus.csv",
  "results_experiment_1_colinglab__CLASS_IT-140M.csv",
  "results_experiment_1_gpt_2_100M.csv",
  "results_experiment_1_phonemetransformers__GPT2-85M-BPE-TXT.csv",
  "results_experiment_1_bbunzeck__grapheme-llama.csv",
  "results_experiment_1_BabyLM-community__babylm-baseline-100m-gpt-bert-masked-focus.csv",
  "results_experiment_1_BabyLM-community__babylm-baseline-100m-gpt-bert-mixed.csv",
  "results_experiment_1_phonemetransformers__GPT2-85M-CHAR-TXT.csv"
)

get_model_name <- function(fn){
  x <- fn
  x <- str_replace(x, "\\.csv$", "")
  x <- str_replace(x, "^results_experiment_\\d+_", "")
  x
}

dat_all <- lapply(file_list, function(f){
  d <- read.csv(f, check.names = TRUE)
  d$model <- get_model_name(f)
  d$File <- f
  d
}) |> bind_rows()

dat_all <- dat_all %>%
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular","Irregular")),
    plurality  = factor(plurality,  levels = c("Plural","Singular")),
    model      = factor(model),
    regularity_num = ifelse(regularity == "Regular", 0, 1),
    plurality_num  = ifelse(plurality  == "Plural", 0, 1),
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


out_file <- "global_model_pluralref_vs_singref.txt"
sink(out_file)

cat("Global model results (Plural ref vs Singular ref)\n")
cat("Generated:", format(Sys.time()), "\n\n")

# --- PLURAL reference ---
dat_plur <- dat_all %>% mutate(plurality = factor(plurality, levels = c("Plural","Singular")))

final_model_plurref <- lmer(
  Surprisal.head ~ 1 + regularity * plurality +
    (1 + plurality | model) + (1 + regularity + plurality | set) + (1 + plurality | Head),
  data = dat_plur,
  REML = TRUE
)

cat("\n--- PLURAL reference ---\n")
print(summary(final_model_plurref))
cat("isSingular:", isSingular(final_model_plurref), "\n\n")

# --- SINGULAR reference ---
dat_sing <- dat_all %>% mutate(plurality = factor(plurality, levels = c("Singular","Plural")))

final_model_singref <- lmer(
  Surprisal.head ~ 1 + regularity * plurality +
    (1 + plurality | model) + (1 + regularity + plurality | set) + (1 + plurality | Head),
  data = dat_sing,
  REML = TRUE
)

cat("\n--- SINGULAR reference ---\n")
print(summary(final_model_singref))
cat("isSingular:", isSingular(final_model_singref), "\n\n")

sink()
cat("Wrote:", out_file, "\n")