library(lme4)
library(lmerTest)
library(dplyr)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_3")

process_data <- function(filename) {
  dat <- read.csv(filename)
  
  dat <- dat %>% 
    mutate(
      regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
      plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
      regularity = factor(regularity, levels = c("Regular", "Irregular")),
      plurality  = factor(plurality,  levels = c("Singular", "Plural")) 
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
  return(dat)
}

sink("stats_results_experiment_3.txt")

# -----------------------------------------------------

data_babble <- process_data("results_experiment_3_babble.csv")

model_babble <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                       (1 + plurality | set) + (1 + regularity_num || Head), 
                     data = data_babble, REML = TRUE)
print(summary(model_babble))

model_babble_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                              (1 + plurality | set) + (1 + regularity_num || Head), 
                            data = data_babble, REML = TRUE)
print(summary(model_babble_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_llama_100 <- process_data("results_experiment_3_babyLlama_100M.csv")

model_llama_100 <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                          (1 + regularity_num || set) + (1 + plurality_num || Head), 
                        data = data_llama_100, REML = TRUE)
print(summary(model_llama_100))

model_llama_100_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                                 (1 + regularity_num || set) + (1 + plurality_num || Head), 
                               data = data_llama_100, REML = TRUE)
print(summary(model_llama_100_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_llama_10 <- process_data("results_experiment_3_babyLlama_10M.csv")

model_llama_10 <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                         (1 + regularity | set) + (1 + plurality | Head), 
                       data = data_llama_10, REML = TRUE)
print(summary(model_llama_10))

model_llama_10_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                                (1 + regularity | set) + (1 + plurality | Head), 
                              data = data_llama_10, REML = TRUE)
print(summary(model_llama_10_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_gpt2_100 <- process_data("results_experiment_3_gpt_2_100M.csv")

model_gpt2_100 <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                         (1 | set) + (1 + plurality_num || Head), 
                       data = data_gpt2_100, REML = TRUE)
print(summary(model_gpt2_100))

model_gpt2_100_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                                (1 | set) + (1 + plurality_num || Head), 
                              data = data_gpt2_100, REML = TRUE)
print(summary(model_gpt2_100_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_gpt2_10 <- process_data("results_experiment_3_gpt_2_10M.csv")

model_gpt2_10 <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                        (1 + regularity_num * plurality_num || set) + (1 + plurality_num || Head), 
                      data = data_gpt2_10, REML = TRUE)
print(summary(model_gpt2_10))

model_gpt2_10_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                               (1 + regularity_num * plurality_num || set) + (1 + plurality_num || Head), 
                             data = data_gpt2_10, REML = TRUE)
print(summary(model_gpt2_10_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_neo_1_3 <- process_data("results_experiment_3_gpt_neo_1_3B.csv")

model_neo_1_3 <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                        (1 + regularity_num || set) + (1 + plurality_num || Head), 
                      data = data_neo_1_3, REML = TRUE)
print(summary(model_neo_1_3))

model_neo_1_3_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                               (1 + regularity_num || set) + (1 + plurality_num || Head), 
                             data = data_neo_1_3, REML = TRUE)
print(summary(model_neo_1_3_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_neo_125 <- process_data("results_experiment_3_gpt_neo_125M.csv")

model_neo_125 <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                        (1 + regularity_num || set) + (1 + plurality_num || Head), 
                      data = data_neo_125, REML = TRUE)
print(summary(model_neo_125))

model_neo_125_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                               (1 + regularity_num || set) + (1 + plurality_num || Head), 
                             data = data_neo_125, REML = TRUE)
print(summary(model_neo_125_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_neo_2_7 <- process_data("results_experiment_3_gpt_neo_2_7B.csv")

model_neo_2_7 <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                        (1 | set) + (1 + plurality | Head), 
                      data = data_neo_2_7, REML = TRUE)
print(summary(model_neo_2_7))

model_neo_2_7_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                               (1 | set) + (1 + plurality | Head), 
                             data = data_neo_2_7, REML = TRUE)
print(summary(model_neo_2_7_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_wee_lg <- process_data("results_experiment_3_gpt_wee_large.csv")

model_wee_lg <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                       (1 + regularity_num || set) + (1 + regularity_num * plurality_num || Head), 
                     data = data_wee_lg, REML = TRUE)
print(summary(model_wee_lg))

model_wee_lg_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                              (1 + regularity_num || set) + (1 + regularity_num * plurality_num || Head), 
                            data = data_wee_lg, REML = TRUE)
print(summary(model_wee_lg_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_wee_md <- process_data("results_experiment_3_gpt_wee_medium.csv")

model_wee_md <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                       (1 + regularity_num * plurality_num || set) + (1 + plurality_num || Head), 
                     data = data_wee_md, REML = TRUE)
print(summary(model_wee_md))

model_wee_md_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                              (1 + regularity_num * plurality_num || set) + (1 + plurality_num || Head), 
                            data = data_wee_md, REML = TRUE)
print(summary(model_wee_md_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_wee_sm <- process_data("results_experiment_3_gpt_wee_small.csv")

model_wee_sm <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                       (1 + regularity_num || set) + (1 + regularity_num + plurality_num || Head), 
                     data = data_wee_sm, REML = TRUE)
print(summary(model_wee_sm))

model_wee_sm_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                              (1 + regularity_num || set) + (1 + regularity_num + plurality_num || Head), 
                            data = data_wee_sm, REML = TRUE)
print(summary(model_wee_sm_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_bert_100 <- process_data("results_experiment_3_LGT_BERT_100M.csv")

model_bert_100 <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                         (1 + plurality_num || set) + (1 | Head), 
                       data = data_bert_100, REML = TRUE)
print(summary(model_bert_100))

model_bert_100_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                                (1 + plurality_num || set) + (1 | Head), 
                              data = data_bert_100, REML = TRUE)
print(summary(model_bert_100_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_bert_10 <- process_data("results_experiment_3_LGT_BERT_10M.csv")

model_bert_10 <- lmer(Surprisal.head ~ 1 + regularity * plurality + 
                        (1 + regularity_num * plurality_num || set) + (1 + regularity + plurality | Head), 
                      data = data_bert_10, REML = TRUE)
print(summary(model_bert_10))

model_bert_10_nested <- lmer(Surprisal.head ~ 1 + regularity / plurality + 
                               (1 + regularity_num * plurality_num || set) + (1 + regularity + plurality | Head), 
                             data = data_bert_10, REML = TRUE)
print(summary(model_bert_10_nested))

sink()