library(lme4)
library(lmerTest)
library(dplyr)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_phonological_study")

process_data <- function(filename) {
  dat <- read.csv(filename, check.names = TRUE)
  
  dat <- dat %>%
    mutate(
      row_id = row_number(),
      set = paste0("set_", ceiling(row_id / 4)),
      pos_in_set = ((row_id - 1) %% 4) + 1,
      
      # Your rule: within each set of 4 rows -> 1st & 3rd = Singular, 2nd & 4th = Plural
      plurality = ifelse(pos_in_set %in% c(1, 3), "Singular", "Plural"),
      
      # Pair 1 = Sibilant, Pair 2 = NonSibilant (use Category if present)
      sibilance = ifelse(grepl("Pair\\s*1", Category), "Sibilant", "NonSibilant"),
      
      sibilance = factor(sibilance, levels = c("NonSibilant", "Sibilant")),
      plurality  = factor(plurality,  levels = c("Singular", "Plural"))
    )
  
  contrasts(dat$sibilance) <- c(-0.5, 0.5)
  contrasts(dat$plurality) <- c(-0.5, 0.5)
  
  dat$sibilance_num <- ifelse(dat$sibilance == "NonSibilant", -0.5, 0.5)
  dat$plurality_num  <- ifelse(dat$plurality  == "Singular",    -0.5, 0.5)
  
  return(dat)
}

sink("stats_results_phonological_study.txt")

# -----------------------------------------------------

data_ph_no_spaces_e2 <- process_data("results_berent&pinker_phonemes_no_spaces_experiment_2.csv")

model_ph_no_spaces_e2 <- lmer(
  Surprisal.head ~ 1 + sibilance * plurality +
    (1 + sibilance | set) + (1 | Head),
  data = data_ph_no_spaces_e2, REML = TRUE
)
print(summary(model_ph_no_spaces_e2))

model_ph_no_spaces_e2_nested <- lmer(
  Surprisal.head ~ 1 + sibilance / plurality +
    (1 + sibilance | set) + (1 | Head),
  data = data_ph_no_spaces_e2, REML = TRUE
)
print(summary(model_ph_no_spaces_e2_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_ph_no_spaces_e3 <- process_data("results_berent&pinker_phonemes_no_spaces_experiment_3.csv")

model_ph_no_spaces_e3 <- lmer(
  Surprisal.head ~ 1 + sibilance * plurality +
    (1 + sibilance_num * plurality_num || set) +
    (1 + sibilance_num * plurality_num || Head),
  data = data_ph_no_spaces_e3, REML = TRUE
)
print(summary(model_ph_no_spaces_e3))

model_ph_no_spaces_e3_nested <- lmer(
  Surprisal.head ~ 1 + sibilance / plurality +
    (1 + sibilance_num * plurality_num || set) +
    (1 + sibilance_num * plurality_num || Head),
  data = data_ph_no_spaces_e3, REML = TRUE
)
print(summary(model_ph_no_spaces_e3_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_ph_with_spaces_e2 <- process_data("results_berent&pinker_phonemes_with_spaces_experiment_2.csv")

model_ph_with_spaces_e2 <- lmer(
  Surprisal.head ~ 1 + sibilance * plurality +
    (1 + plurality_num || set) +
    (1 + sibilance_num * plurality_num || Head),
  data = data_ph_with_spaces_e2, REML = TRUE
)
print(summary(model_ph_with_spaces_e2))

model_ph_with_spaces_e2_nested <- lmer(
  Surprisal.head ~ 1 + sibilance / plurality +
    (1 + plurality_num || set) +
    (1 + sibilance_num * plurality_num || Head),
  data = data_ph_with_spaces_e2, REML = TRUE
)
print(summary(model_ph_with_spaces_e2_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_ph_with_spaces_e3 <- process_data("results_berent&pinker_phonemes_with_spaces_experiment_3.csv")

model_ph_with_spaces_e3 <- lmer(
  Surprisal.head ~ 1 + sibilance * plurality +
    (1 | set) + (1 + sibilance * plurality | Head),
  data = data_ph_with_spaces_e3, REML = TRUE
)
print(summary(model_ph_with_spaces_e3))

model_ph_with_spaces_e3_nested <- lmer(
  Surprisal.head ~ 1 + sibilance / plurality +
    (1 | set) + (1 + sibilance * plurality | Head),
  data = data_ph_with_spaces_e3, REML = TRUE
)
print(summary(model_ph_with_spaces_e3_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_text_with_spaces_e2 <- process_data("results_berent&pinker_text_with_spaces_experiment_2.csv")

model_text_with_spaces_e2 <- lmer(
  Surprisal.head ~ 1 + sibilance * plurality +
    (1 | set) + (1 + sibilance_num || Head),
  data = data_text_with_spaces_e2, REML = TRUE
)
print(summary(model_text_with_spaces_e2))

model_text_with_spaces_e2_nested <- lmer(
  Surprisal.head ~ 1 + sibilance / plurality +
    (1 | set) + (1 + sibilance_num || Head),
  data = data_text_with_spaces_e2, REML = TRUE
)
print(summary(model_text_with_spaces_e2_nested))

cat("\n\n# -----------------------------------------------------\n\n")

# -----------------------------------------------------

data_text_with_spaces_e3 <- process_data("results_berent&pinker_text_with_spaces_experiment_3.csv")

model_text_with_spaces_e3 <- lmer(
  Surprisal.head ~ 1 + sibilance * plurality +
    (1 + sibilance_num * plurality_num || set) +
    (1 + plurality | Head),
  data = data_text_with_spaces_e3, REML = TRUE
)
print(summary(model_text_with_spaces_e3))

model_text_with_spaces_e3_nested <- lmer(
  Surprisal.head ~ 1 + sibilance / plurality +
    (1 + sibilance_num * plurality_num || set) +
    (1 + plurality | Head),
  data = data_text_with_spaces_e3, REML = TRUE
)
print(summary(model_text_with_spaces_e3_nested))

sink()
