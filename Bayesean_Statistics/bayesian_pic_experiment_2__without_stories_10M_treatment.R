library(lme4)
library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)


setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_berent&pinker/experiment_2_without_stories/10M")

file_list <- list.files(pattern = "\\.csv$", full.names = FALSE)

get_model_name <- function(fn){
  x <- fn
  x <- str_replace(x, "\\.csv$", "")
  x <- str_replace(x, "^results_experiment_\\d+_", "")
  x
}

dat <- lapply(file_list, function(f){
  d <- read.csv(f, check.names = TRUE)
  d$model <- get_model_name(f)
  d$File <- f
  d
}) |> bind_rows()

dat <- dat %>%
  mutate(
    type = ifelse(grepl("Sibilant", Category), "Sibilant", "Regular"),
    plurality = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    type = factor(type, levels = c("Regular", "Sibilant")),
    plurality = factor(plurality, levels = c("Plural", "Singular"))
  )


dat <- dat %>%
  mutate(
    set = case_when(
      Non.Head %in% c("blaze","blazes","spark","sparks") ~ "set_blaze_spark",
      Non.Head %in% c("breeze","breezes","storm","storms") ~ "set_breeze_storm",
      Non.Head %in% c("tax","taxes","toll","tolls") ~ "set_tax_toll",
      Non.Head %in% c("sex","sexes","gender","genders") ~ "set_sex_gender",
      Non.Head %in% c("vase","vases","pot","pots") ~ "set_vase_pot",
      Non.Head %in% c("hoax","hoaxes","joke","jokes") ~ "set_hoax_joke",
      Non.Head %in% c("phase","phases","step","steps") ~ "set_phase_step",
      Non.Head %in% c("hose","hoses","pipe","pipes") ~ "set_hose_pipe",
      Non.Head %in% c("fox","foxes","wolf","wolves") ~ "set_fox_wolf",
      Non.Head %in% c("mix","mixes","blend","blends") ~ "set_mix_blend",
      Non.Head %in% c("nose","noses","thigh","thighs") ~ "set_nose_thigh",
      Non.Head %in% c("cause","causes","loss","losses") ~ "set_cause_loss",
      Non.Head %in% c("gaze","gazes","stare","stares") ~ "set_gaze_stare",
      Non.Head %in% c("clause","clauses","article","articles") ~ "set_clause_article",
      Non.Head %in% c("maze","mazes","web","webs") ~ "set_maze_web",
      Non.Head %in% c("quiz","quizzes","puzzle","puzzles") ~ "set_quiz_puzzle",
      Non.Head %in% c("fax","faxes","copy","copies") ~ "set_fax_copy",
      Non.Head %in% c("size","sizes","shape","shapes") ~ "set_size_shape",
      Non.Head %in% c("praise","praises","compliment","compliments") ~ "set_praise_compliment",
      Non.Head %in% c("prize","prizes","award","awards") ~ "set_prize_award",
      Non.Head %in% c("box","boxes","pack","packs") ~ "set_box_pack",
      Non.Head %in% c("rose","roses","flower","flowers") ~ "set_rose_flower",
      Non.Head %in% c("bruise","bruises","sore","sores") ~ "set_bruise_sore",
      Non.Head %in% c("rise","rises","drop","drops") ~ "set_rise_drop",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(set))

### Running the frequentist model for comparison
m_freq <- lmer(Surprisal.head ~ 1 + type * plurality +
                 (1 + type * plurality | model) + (1 + type * plurality | set) + (1 + type * plurality | Head),
               data = dat)
summary(m_freq) |> print(cor = F)


### Priors
priors_surprisal <-
  prior(normal(10, 5), class = Intercept, lb = 0) +
  prior(normal(0, 1), class = b) +
  prior(exponential(2.5), class = sd, group = model, coef = Intercept) +
  prior(exponential(2.5), class = sd, group = set, coef = Intercept) +
  prior(exponential(2.5), class = sd, group = Head, coef = Intercept) +
  prior(exponential(5), class = sd, group = model) +
  prior(exponential(5), class = sd, group = set) +
  prior(exponential(5), class = sd, group = Head) +
  prior(lkj(1), class=cor) +
  prior(exponential(1), class=sigma)

m <- brm(Surprisal.head ~ 1 + type * plurality +
           (1 + type * plurality | model) +
           (1 + type * plurality | set) +
           (1 + type * plurality | Head),
         data = dat,
         prior = priors_surprisal,
         sample_prior = "yes",
         chains = 4, iter = 12000, warmup = 2000,
         cores = 4,
         backend = "cmdstanr",
         file = "C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/Bayesean_Statistics/surprisal_gaussian_experiment_2_without_stories_10M_treatment")
summary(m)