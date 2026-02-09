library(lme4)
library(lmerTest)
library(dplyr)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1")

# Get list of all CSV files in the folder
#file_list <- list.files(pattern = "\\.csv$")

# currently it is reading all the files in the folder. But alternatively you can add a list of files:
file_list <- c("results_experiment_1_phonemetransformers__GPT2-85M-CHAR-TXT-SPACELESS.csv" 
              )

#-------------------------------------------------------------------------------------------------

### SETUP: FUNCTIONS

### Convergence and singularity check. 
hasConverged <- function (mm) {
  if (is.null(unlist(mm@optinfo$conv$lme4))) {
    return(1)   # converged
  } else {
    if (isSingular(mm)) return(0) # singular (boundary)
    return(-1) # did not converge (non-singular)
  }
}

### Fitting each model and obtaining the respective AIC. 
fit_model <- function(fml, current_dat){
  out <- tryCatch({
    m <- lmer(fml, current_dat, REML = TRUE)
    # We MUST return a list because the rest of your code looks for '$AIC' and '$convergence'
    list(
      AIC = as.numeric(AIC(m)),
      convergence = hasConverged(m),
      error = NA_character_
    )
  }, error = function(e){
    # If it crashes, we need to return this structure so the loop doesn't die
    list(
      AIC = NA_real_,
      convergence = -99,
      error = conditionMessage(e)
    )
  })
  out
}

# helper: build formulas from random-effects strings
build_formulas <- function(re_strings){
  sapply(
    re_strings,
    \(x) as.formula(paste0("Surprisal.head ~ 1 + regularity * plurality + ", x))
  ) |> unname()
}

# helper: fit a list and return a clean 1-row-per-model data.frame
fit_formula_list <- function(formulas, current_dat){
  rows <- vector("list", length(formulas))
  for(i in seq_along(formulas)){
    res <- fit_model(formulas[[i]], current_dat)
    
    # Clean formula string: prevents line breaks and extra spaces
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

### MODEL SELECTION LOGIC

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

### MAIN LOOP

cat("Found", length(file_list), "files to process.\n\n")

# List to store the winning model for each file
winners_list <- list()

for (current_file in file_list) {
  
  cat("Processing:", current_file, "...\n")
  
  # Load data
  dat <- read.csv(current_file, check.names = TRUE)
  
# -----------------------------------------------------------------------------------------------------------
  
### DATA PREPARATION

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
  
### MODEL FITTING

  # GROUP 1: CORRELATED set (|) + CORRELATED Head (|)
  re_g1 <- apply(expand.grid(rs_set, rs_head), 1, \(x) paste(x, collapse = " + "))
  fr_g1 <- build_formulas(re_g1)
  df_g1 <- fit_formula_list(fr_g1, dat)
  
  # GROUP 2: UNCORRELATED set (||) + UNCORRELATED Head (||)
  re_g2 <- apply(expand.grid(rs_set_uncorr, rs_head_uncorr), 1, \(x) paste(x, collapse = " + "))
  re_g2 <- re_g2[re_g2 != "(1 | set) + (1 | Head)"]  # as you wanted
  fr_g2 <- build_formulas(re_g2)
  df_g2 <- fit_formula_list(fr_g2, dat)
  
  # GROUP 3: CORRELATED set (|) + UNCORRELATED Head (||)
  re_g3 <- apply(expand.grid(rs_set, rs_head_uncorr), 1, \(x) paste(x, collapse = " + "))
  re_g3 <- re_g3[re_g3 != "(1 | set) + (1 | Head)"]  # avoid duplicate baseline
  fr_g3 <- build_formulas(re_g3)
  df_g3 <- fit_formula_list(fr_g3, dat)
  
  # GROUP 4: UNCORRELATED set (||) + CORRELATED Head (|)
  re_g4 <- apply(expand.grid(rs_set_uncorr, rs_head), 1, \(x) paste(x, collapse = " + "))
  re_g4 <- re_g4[re_g4 != "(1 | set) + (1 | Head)"]  # avoid duplicate baseline
  fr_g4 <- build_formulas(re_g4)
  df_g4 <- fit_formula_list(fr_g4, dat)
  
  all_results <- rbind(df_g1, df_g2, df_g3, df_g4)
  # add print(all results) to add the full prints of what is hapening.
  
  # Find the winner for this specific file
  # Filter: Convergence == 1 (Success) AND lowest AIC
  winner <- all_results[all_results$convergence == 1, ]
  
  if (nrow(winner) > 0) {
    winner <- winner[which.min(winner$AIC), ]
    winner$File <- current_file  # Add filename so we know which file this is
    winners_list[[current_file]] <- winner
  } else {
    # Fallback if NO models converged for this file
    cat("  WARNING: No models converged for file:", current_file, "\n")
  }
}

# -----------------------------------------------------------------------------------------------------------

### FINAL OUTPUT

cat("\n--- PROCESSING COMPLETE ---\n\n")

final_table <- do.call(rbind, winners_list)

# Formatting: Wide view, no row names, Formula 2nd (to the left)
options(width = 500)
rownames(final_table) <- NULL
final_table <- final_table[, c("File", "formula", "AIC")]

print(final_table, right = FALSE, row.names = FALSE)


final_model <- lmer(
  Surprisal.head ~ 1 + regularity * plurality +  (1 + regularity + plurality | set) + (1 + plurality_num || Head), 
  data = dat, 
  REML = TRUE
)

summary(final_model)
