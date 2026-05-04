library(lme4)
library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)


setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/100M")

file_list <- c(
  "results_experiment_1_gpt_2_100M.csv",
  "results_experiment_1_babyLlama_100M.csv",
  "results_experiment_1_babble_txt_BPE_with_spaces.csv",
  "results_experiment_1_CLASS_IT.csv",
  "results_experiment_1_OPT_100M.csv",
  "results_experiment_1_gpt_bert_100M_causal.csv",
  "results_experiment_1_gpt_bert_100M_mixed.csv",
  "results_experiment_1_gpt_bert_100M_masked.csv"
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
  d$File <- f
  d
}) |> bind_rows()

dat <- dat %>%
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular", "Irregular")),
    plurality  = factor(plurality,  levels = c("Plural", "Singular"))
  )


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

### Running the frequentist model for comparison
m_freq <- lmer(Surprisal.head ~ 1 + regularity * plurality +
                 (1 + regularity * plurality | model) + (1 + regularity * plurality | set) + (1 + regularity * plurality | Head),
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

m <- brm(Surprisal.head ~ 1 + regularity * plurality +
           (1 + regularity * plurality | model) +
           (1 + regularity * plurality | set) +
           (1 + regularity * plurality | Head),
         data = dat,
         prior = priors_surprisal,
         sample_prior = "yes",
         chains = 4, iter = 12000, warmup = 2000,
         cores = 4,
         backend = "cmdstanr",
         file = "C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/Bayesean_Statistics/surprisal_gaussian_experiment_1_100M_treatment")
summary(m)