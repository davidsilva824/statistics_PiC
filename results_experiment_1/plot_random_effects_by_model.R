library(lme4)
library(lmerTest)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)

# this folder contains the results files obtained from study_pic_4
setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/10M")

models <- data.frame(
  File = c(
    "results_experiment_1_babyLlama_10M.csv",
    "results_experiment_1_babyLlama_2_10M.csv",
    "results_experiment_1_distill_tree__360_10M.csv.csv",
    "results_experiment_1_distill_tree__58_10M.csv",
    "results_experiment_1_distill_tree__95_10M.csv",
    "results_experiment_1_gpt_2_10M.csv",
    "results_experiment_1_gpt_bert_10M_causal.csv",
    "results_experiment_1_gpt_bert_10M_masked.csv",
    "results_experiment_1_gpt_bert_10M_mixed.csv",
    "results_experiment_1_gpt_wee_large.csv",
    "results_experiment_1_gpt_wee_medium.csv",
    "results_experiment_1_gpt_wee_small.csv",
    "results_experiment_1_MOEP.csv",
    "results_experiment_1_MOP.csv",
    "results_experiment_1_OPT_10M.csv",
    "results_experiment_1_what_if_baseline.csv",
    "results_experiment_1_ZLATA.csv"
  ),
  formula = c(
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity | set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num * plurality_num || set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity | set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity | set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity | set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num * plurality_num || set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 | set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num || set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num * plurality_num || set) + (1 + plurality | Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num || set) + (1 + regularity_num * plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num * plurality_num || set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num || set) + (1 + regularity_num + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num + plurality_num || set) + (1 | Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + plurality_num || set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num * plurality_num || set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 | set) + (1 + regularity_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num * plurality_num || set) + (1 + plurality_num || Head)"
  ),
  stringsAsFactors = FALSE
)

add_set <- function(dat){
  dat %>%
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

get_model_name <- function(fn){
  x <- fn
  x <- str_replace(x, "\\.csv$", "")
  x <- str_replace(x, "^results_experiment_\\d+_", "")
  x
}

results <- vector("list", nrow(models))

for(i in seq_len(nrow(models))){
  file <- models$File[i]
  fml  <- models$formula[i]
  
  dat <- read.csv(file, check.names = TRUE)
  
  dat <- dat %>%
    mutate(
      regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
      plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
      regularity = factor(regularity, levels = c("Regular", "Irregular")),
      plurality  = factor(plurality, levels = c("Singular", "Plural"))
    )
  
  contrasts(dat$regularity) <- c(-0.5, 0.5)
  contrasts(dat$plurality)  <- c(-0.5, 0.5)
  
  dat$regularity_num <- ifelse(dat$regularity == "Regular", -0.5, 0.5)
  dat$plurality_num  <- ifelse(dat$plurality  == "Singular", -0.5, 0.5)
  
  dat <- add_set(dat) %>% filter(!is.na(set))
  
  m <- tryCatch(
    lmer(as.formula(fml), dat, REML = TRUE),
    error = function(e) e
  )
  
  if(inherits(m, "error")){
    results[[i]] <- data.frame(
      model = get_model_name(file),
      estimate = NA_real_,
      SE = NA_real_,
      lower95 = NA_real_,
      upper95 = NA_real_,
      stringsAsFactors = FALSE
    )
    next
  }
  
  coefs <- as.data.frame(coef(summary(m)))
  coefs$term <- rownames(coefs)
  
  interaction_row <- coefs[
    grepl(":", coefs$term) &
      grepl("regularity", coefs$term) &
      grepl("plurality", coefs$term),
  ]
  
  if(nrow(interaction_row) == 0){
    results[[i]] <- data.frame(
      model = get_model_name(file),
      estimate = NA_real_,
      SE = NA_real_,
      lower95 = NA_real_,
      upper95 = NA_real_,
      stringsAsFactors = FALSE
    )
    next
  }
  
  estimate <- interaction_row$Estimate[1]
  SE       <- interaction_row$`Std. Error`[1]
  
  results[[i]] <- data.frame(
    model = get_model_name(file),
    estimate = estimate,
    SE = SE,
    lower95 = estimate - 1.96 * SE,
    upper95 = estimate + 1.96 * SE,
    stringsAsFactors = FALSE
  )
}

plot_dat <- bind_rows(results) %>%
  filter(!is.na(estimate)) %>%
  mutate(model = fct_reorder(model, estimate))

p <- ggplot(plot_dat, aes(x = model, y = estimate)) +
  coord_flip() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue") +
  labs(
    x = "Model",
    y = "Interaction estimate",
    title = "Interaction estimates by model"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 14, colour = "gray28")
  )

print(p)

write.csv(plot_dat, "interaction_forest_plot_values_10M.csv", row.names = FALSE)

ggsave(
  "interaction_forest_plot_by_model_10M.png",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)