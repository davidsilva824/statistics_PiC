library(lme4)
library(lmerTest)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggeffects)

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

all_preds <- vector("list", nrow(models))

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
  
  if(inherits(m, "error")) next
  
  pred <- as.data.frame(ggemmeans(m, c("regularity", "plurality")))
  
  pred$model <- get_model_name(file)
  pred$regularity <- pred$x
  pred$plurality  <- pred$group
  
  all_preds[[i]] <- pred[, c("model", "regularity", "plurality", "predicted", "std.error", "conf.low", "conf.high")]
}

plot_dat <- bind_rows(all_preds)

model_order <- get_model_name(models$File)
plot_dat$model <- factor(plot_dat$model, levels = model_order)

p <- ggplot(
  plot_dat,
  aes(x = regularity, y = predicted, group = plurality, colour = plurality)
) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.08) +
  facet_wrap(~ model, ncol = 4) +
  labs(
    x = "Regularity",
    y = "Predicted head surprisal",
    colour = "Plurality",
    title = "Predicted means by model"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 12, colour = "gray28"),
    strip.text = element_text(size = 10)
  )

print(p)

write.csv(plot_dat, "predicted_means_by_model_10M.csv", row.names = FALSE)

ggsave(
  "predicted_means_by_model_10M.png",
  plot = p,
  width = 12,
  height = 10,
  dpi = 300
)