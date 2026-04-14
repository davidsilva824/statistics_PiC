library(lme4)
library(lmerTest)
library(dplyr)
library(stringr)
library(ggplot2)

options(scipen = 9999)
options(digits = 3)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/10M")

models <- read.csv(
  "winning_models_results.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

models <- models[, c("File", "formula")]
models$File <- basename(models$File)
models <- models[file.exists(models$File), ]

get_model_name <- function(fn){
  x <- fn
  x <- str_replace(x, "(\\.csv)+$", "")
  x <- str_replace(x, "^results_experiment_\\d+_", "")
  x
}

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

# Only the selected case: PR
prep_PR <- function(dat){
  dat <- dat %>%
    mutate(
      regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
      plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
      regularity = factor(regularity, levels = c("Regular","Irregular")),
      plurality  = factor(plurality,  levels = c("Plural","Singular"))
    )
  dat$regularity_num <- ifelse(dat$regularity == "Regular", 0, 1)
  dat$plurality_num  <- ifelse(dat$plurality  == "Plural", 0, 1)
  add_set(dat)
}

results <- vector("list", nrow(models))

for(i in seq_len(nrow(models))){
  file <- models$File[i]
  fml  <- models$formula[i]
  model_name <- get_model_name(file)
  
  dat <- read.csv(file, check.names = TRUE)
  dat <- prep_PR(dat)
  
  m <- tryCatch(
    lmer(as.formula(fml), dat, REML = TRUE),
    error = function(e) e
  )
  
  if(inherits(m, "error")){
    results[[i]] <- data.frame(
      model = model_name,
      effect = c(
        "Regularity × plurality interaction",
        "Regular plural effect\n(e.g., ducks trader vs. duck trader)",
        "Irregular plural effect\n(e.g., geese trader vs. goose trader)"
      ),
      estimate = NA_real_,
      SE = NA_real_,
      stringsAsFactors = FALSE
    )
    next
  }
  
  b <- fixef(m)
  V <- as.matrix(vcov(m))
  
  beta_p <- unname(b["pluralitySingular"])
  beta_i <- unname(b["regularityIrregular:pluralitySingular"])
  
  se_p <- sqrt(V["pluralitySingular", "pluralitySingular"])
  se_i <- sqrt(V["regularityIrregular:pluralitySingular",
                 "regularityIrregular:pluralitySingular"])
  
  # Irregular effect = -(pluralitySingular + interaction)
  var_irr <- V["pluralitySingular", "pluralitySingular"] +
    V["regularityIrregular:pluralitySingular",
      "regularityIrregular:pluralitySingular"] +
    2 * V["pluralitySingular",
          "regularityIrregular:pluralitySingular"]
  
  results[[i]] <- data.frame(
    model = model_name,
    effect = c(
      "Regularity × plurality interaction",
      "Regular plural effect\n(e.g., ducks trader vs. duck trader)",
      "Irregular plural effect\n(e.g., geese trader vs. goose trader)"
    ),
    estimate = c(
      beta_i,
      -beta_p,
      -(beta_p + beta_i)
    ),
    SE = c(
      se_i,
      se_p,
      sqrt(var_irr)
    ),
    stringsAsFactors = FALSE
  )
}

plot_dat <- bind_rows(results) %>%
  mutate(
    lower95 = estimate - 1.96 * SE,
    upper95 = estimate + 1.96 * SE,
    effect = factor(
      effect,
      levels = c(
        "Regularity × plurality interaction",
        "Regular plural effect\n(e.g., ducks trader vs. duck trader)",
        "Irregular plural effect\n(e.g., geese trader vs. goose trader)"
      )
    )
  )

interaction_order <- plot_dat %>%
  filter(effect == "Regularity × plurality interaction") %>%
  arrange(estimate) %>%
  pull(model)

plot_dat$model <- factor(plot_dat$model, levels = interaction_order)

write.csv(plot_dat, "forest_plot_treatment_values_10M.csv", row.names = FALSE)

p <- ggplot(plot_dat, aes(x = model, y = estimate)) +
  coord_flip() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue") +
  facet_wrap(~ effect, ncol = 3) +
  labs(
    x = "Model",
    y = "Estimate",
  ) +
  theme_light() +
  theme(
    text = element_text(size = 20, colour = "gray28"),
    strip.text = element_text(size = 19),
    axis.text.y = element_text(size = 15),
    plot.title = element_text(hjust = 0.5)
  )

print(p)

ggsave(
  "forest_plot_treatment_3panels_10M.png",
  plot = p,
  width = 16,
  height = 7,
  dpi = 300
)