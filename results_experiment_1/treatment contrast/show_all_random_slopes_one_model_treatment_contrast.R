library(lme4)
library(lmerTest)
library(dplyr)

# this folder contains the results files obtained from study_pic_4: https://github.com/davidsilva824/study_pic_4 
setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/100M")

# -------------------------------------------------------------------------------------------------

# Choose one file only
current_file <- "results_experiment_1_phoneme_llama_no_spaces.csv"

# Number of best solutions to show
n_top <- 10

# -------------------------------------------------------------------------------------------------

### ### FUNCTIONS

# a function to check if a model converges 
hasConverged <- function (mm) {
  if (is.null(unlist(mm@optinfo$conv$lme4))) {
    return(1)   # converged
  } else {
    if (isSingular(mm)) return(0) # singular (boundary)
    return(-1) # did not converge (non-singular)
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

# function to add the fixed part of the formula to all random-effects combinations
build_formulas <- function(re_strings){
  sapply(
    re_strings,
    \(x) as.formula(paste0("Surprisal.head ~ 1 + regularity * plurality + ", x))
  ) |> unname()
}

# This function fits all the formulas and returns a results table.
fit_formula_list <- function(formulas, current_dat){
  rows <- vector("list", length(formulas))
  for(i in seq_along(formulas)){
    res <- fit_model(formulas[[i]], current_dat)
    
    clean_formula <- paste(deparse(formulas[[i]], width.cutoff = 500), collapse = " ")
    
    rows[[i]] <- data.frame(
      AIC = res$AIC,
      convergence = res$convergence,
      formula = clean_formula,
      error = res$error,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# -----------------------------------------------------------------------------------------------------------

### Building the random-effects terms.

# Define Random Slopes (factor-coded, correlated)
rs <- c("",
        " + regularity",
        " + plurality",
        " + regularity + plurality",
        " + regularity * plurality")

rs_set  <- sapply(rs, \(x) paste0("(1", x, " | set)")) |> unname()
rs_head <- sapply(rs, \(x) paste0("(1", x, " | Head)")) |> unname()

# Numeric (uncorrelated) options
rs_num <- c(" + regularity_num",
            " + plurality_num",
            " + regularity_num + plurality_num",
            " + regularity_num * plurality_num")

# allow intercept-only, OR numeric slopes with ||
rs_set_uncorr  <- c("(1 | set)",  sapply(rs_num, \(x) paste0("(1", x, " || set)"))  |> unname())
rs_head_uncorr <- c("(1 | Head)", sapply(rs_num, \(x) paste0("(1", x, " || Head)")) |> unname())

# -----------------------------------------------------------------------------------------------------------

### MAIN

cat("Processing:", current_file, "...\n")

# Load data
dat <- read.csv(current_file, check.names = TRUE)

# -----------------------------------------------------------------------------------------------------------

### Data preparation (TREATMENT CONTRASTS VERSION)

dat <- dat %>% 
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular", "Irregular")),
    plurality  = factor(plurality,  levels = c("Plural", "Singular"))
  )

# Keep numeric coding for || random slopes
dat$regularity_num <- ifelse(dat$regularity == "Regular", 0, 1)
dat$plurality_num  <- ifelse(dat$plurality  == "Plural", 0, 1)

# Create 'set'
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

# -----------------------------------------------------------------------------------------------------------

### Generating and fitting all the combinations

# GROUP 1: CORRELATED set (|) + CORRELATED Head (|)
re_g1 <- apply(expand.grid(rs_set, rs_head), 1, \(x) paste(x, collapse = " + "))
fr_g1 <- build_formulas(re_g1)
df_g1 <- fit_formula_list(fr_g1, dat)

# GROUP 2: UNCORRELATED set (||) + UNCORRELATED Head (||)
re_g2 <- apply(expand.grid(rs_set_uncorr, rs_head_uncorr), 1, \(x) paste(x, collapse = " + "))
re_g2 <- re_g2[re_g2 != "(1 | set) + (1 | Head)"]
fr_g2 <- build_formulas(re_g2)
df_g2 <- fit_formula_list(fr_g2, dat)

# GROUP 3: CORRELATED set (|) + UNCORRELATED Head (||)
re_g3 <- apply(expand.grid(rs_set, rs_head_uncorr), 1, \(x) paste(x, collapse = " + "))
re_g3 <- re_g3[re_g3 != "(1 | set) + (1 | Head)"]
fr_g3 <- build_formulas(re_g3)
df_g3 <- fit_formula_list(fr_g3, dat)

# GROUP 4: UNCORRELATED set (||) + CORRELATED Head (|)
re_g4 <- apply(expand.grid(rs_set_uncorr, rs_head), 1, \(x) paste(x, collapse = " + "))
re_g4 <- re_g4[re_g4 != "(1 | set) + (1 | Head)"]
fr_g4 <- build_formulas(re_g4)
df_g4 <- fit_formula_list(fr_g4, dat)

all_results <- rbind(df_g1, df_g2, df_g3, df_g4)

# -----------------------------------------------------------------------------------------------------------

### Final Output: top n converged models by AIC

top_results <- all_results[all_results$convergence == 1, ]

if (nrow(top_results) == 0) {
  cat("WARNING: No models converged for file:", current_file, "\n")
} else {
  top_results <- top_results[order(top_results$AIC), ]
  top_results <- head(top_results, n_top)
  top_results$File <- current_file
  
  options(width = 500)
  rownames(top_results) <- NULL
  top_results <- top_results[, c("File", "formula", "AIC")]
  
  write.csv(top_results, "top_n_models_results.csv", row.names = FALSE)
  
  cat("\n--- TOP", n_top, "MODELS BY AIC ---\n\n")
  print(top_results, right = FALSE, row.names = FALSE)
}