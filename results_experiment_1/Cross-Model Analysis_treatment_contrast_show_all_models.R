library(lme4)
library(lmerTest)
library(dplyr)
library(stringr)

# this folder contains the results files obtained from study_pic_4: https://github.com/davidsilva824/study_pic_4 
setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/100M")

# To read one or more specific files in the folder:
file_list <- c(
  "results_experiment_1_babyLlama_100M.csv",
  "results_experiment_1_OPT_100M.csv",
  "results_experiment_1_gpt_bert_100M_causal.csv",
  "results_experiment_1_CLASS_IT.csv",
  "results_experiment_1_gpt_2_100M.csv",
  "results_experiment_1_babble_txt_BPE_with_spaces.csv",
  "results_experiment_1_gpt_bert_100M_masked.csv",
  "results_experiment_1_gpt_bert_100M_mixed.csv"
)

# -------------------------------------------------------------------------------------------
### FUNCTIONS

# a function to check if a model converges 
hasConverged <- function(mm) {
  if (is.null(unlist(mm@optinfo$conv$lme4))) {
    return(1)   # converged (no lme4 convergence messages)
  } else {
    if (isSingular(mm)) return(0) # singular (boundary)
    return(-1) # other convergence problems
  }
}

# Function to fit the mixed models safely
fit_model <- function(fml, current_dat){
  out <- tryCatch({
    m <- lmer(fml, current_dat, REML = TRUE)
    list(
      AIC = as.numeric(AIC(m)),
      convergence = hasConverged(m),
      error = NA_character_
    )
  }, error = function(e){
    list(
      AIC = NA_real_,
      convergence = -99,
      error = conditionMessage(e)
    )
  })
  out
}

# function to add the fixed part to all random-effects combinations
build_formulas <- function(re_strings){
  sapply(
    re_strings,
    \(x) as.formula(paste0("Surprisal.head ~ 1 + regularity * plurality + ", x))
  ) |> unname()
}

# This function fits all the formulas and returns a results table (WITH family column)
fit_formula_list <- function(formulas, current_dat, family_label){
  rows <- vector("list", length(formulas))
  for(i in seq_along(formulas)){
    res <- fit_model(formulas[[i]], current_dat)
    clean_formula <- paste(deparse(formulas[[i]], width.cutoff = 500), collapse = " ")
    rows[[i]] <- data.frame(
      AIC = res$AIC,
      convergence = res$convergence,
      formula = clean_formula,
      family = family_label,
      error = res$error,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# function to convert the file name into the model name
get_model_name <- function(fn){
  x <- fn
  x <- str_replace(x, "\\.csv$", "")
  x <- str_replace(x, "^results_experiment_\\d+_", "")
  x
}

# -------------------------------------------------------------------------------------------
### Load + stack all CSVs

out_file <- "global_model_selection_results.txt"
if (file.exists(out_file)) file.remove(out_file)
sink(out_file, split = TRUE)

cat("Found", length(file_list), "files to process.\n\n")

dat_all <- lapply(file_list, function(f){
  d <- read.csv(f, check.names = TRUE)
  d$model <- get_model_name(f)
  d$File <- f
  d
}) |> bind_rows()

# -------------------------------------------------------------------------------------------
### Data preparation (treatment; Plural reference)

dat_all <- dat_all %>%
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular", "Irregular")),
    plurality  = factor(plurality,  levels = c("Plural", "Singular")),
    model      = factor(model)
  )

dat_all$regularity_num <- ifelse(dat_all$regularity == "Regular", 0, 1)
dat_all$plurality_num  <- ifelse(dat_all$plurality  == "Plural", 0, 1)

dat_all <- dat_all %>%
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

# drop NA sets
dat_all <- dat_all %>% filter(!is.na(set))

# -------------------------------------------------------------------------------------------
### Random-effects menus

rs <- c("",
        " + regularity",
        " + plurality",
        " + regularity + plurality",
        " + regularity * plurality")

# factor-coded, correlated
rs_model <- sapply(rs, \(x) paste0("(1", x, " | model)")) |> unname()
rs_set   <- sapply(rs, \(x) paste0("(1", x, " | set)"))   |> unname()
rs_head  <- sapply(rs, \(x) paste0("(1", x, " | Head)"))  |> unname()

# numeric, uncorrelated
rs_num <- c(" + regularity_num",
            " + plurality_num",
            " + regularity_num + plurality_num",
            " + regularity_num * plurality_num")

rs_model_uncorr <- c("(1 | model)", sapply(rs_num, \(x) paste0("(1", x, " || model)")) |> unname())
rs_set_uncorr   <- c("(1 | set)",   sapply(rs_num, \(x) paste0("(1", x, " || set)"))   |> unname())
rs_head_uncorr  <- c("(1 | Head)",  sapply(rs_num, \(x) paste0("(1", x, " || Head)"))  |> unname())

# -------------------------------------------------------------------------------------------
### Generate + fit all combinations

# G1: all correlated
re_g1 <- apply(expand.grid(rs_model, rs_set, rs_head), 1, \(x) paste(x, collapse=" + "))
fr_g1 <- build_formulas(re_g1)
df_g1 <- fit_formula_list(fr_g1, dat_all, "model_|__set_|__Head_|")

# G2: all uncorrelated
re_g2 <- apply(expand.grid(rs_model_uncorr, rs_set_uncorr, rs_head_uncorr), 1, \(x) paste(x, collapse=" + "))
re_g2 <- re_g2[re_g2 != "(1 | model) + (1 | set) + (1 | Head)"]
fr_g2 <- build_formulas(re_g2)
df_g2 <- fit_formula_list(fr_g2, dat_all, "model_||__set_||__Head_||")

# G3: model correlated, set/head uncorrelated
re_g3 <- apply(expand.grid(rs_model, rs_set_uncorr, rs_head_uncorr), 1, \(x) paste(x, collapse=" + "))
re_g3 <- re_g3[re_g3 != "(1 | model) + (1 | set) + (1 | Head)"]
fr_g3 <- build_formulas(re_g3)
df_g3 <- fit_formula_list(fr_g3, dat_all, "model_|__set_||__Head_||")

# G4: model uncorrelated, set/head correlated
re_g4 <- apply(expand.grid(rs_model_uncorr, rs_set, rs_head), 1, \(x) paste(x, collapse=" + "))
re_g4 <- re_g4[re_g4 != "(1 | model) + (1 | set) + (1 | Head)"]
fr_g4 <- build_formulas(re_g4)
df_g4 <- fit_formula_list(fr_g4, dat_all, "model_||__set_|__Head_|")

all_results <- rbind(df_g1, df_g2, df_g3, df_g4)

# keep one row per formula (avoid duplicates)
all_results <- all_results[!duplicated(all_results$formula), ]

# order: NA AIC (errors) go to bottom
all_results <- all_results[order(is.na(all_results$AIC), all_results$AIC), ]

# PRINT FULL LIST (like your single-file script)
print(all_results)

# -------------------------------------------------------------------------------------------
### Winner (best converged)
winner <- all_results[all_results$convergence == 1, ]
if(nrow(winner) == 0) stop("No models converged in the global fit.")
winner <- winner[which.min(winner$AIC), ]

cat("\n--- GLOBAL MODEL SELECTION COMPLETE ---\n\n")
print(winner[, c("AIC","convergence","formula","family")], row.names = FALSE)

sink()