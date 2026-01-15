library(lme4)
library(lmerTest)
library(dplyr)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_phonological_study")

# Get list of all CSV files for THIS experiment in the folder
file_list <- list.files(pattern = "berent.*\\.csv$", ignore.case = TRUE)

# currently it is reading all the files in the folder. But alternatively you can add a list of files:
#file_list <- c("results_berent&pinker_text_with_spaces_experiment_3.csv", ...)

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

# helper: build formulas from random-effects strings
build_formulas <- function(re_strings){
  sapply(
    re_strings,
    \(x) as.formula(paste0("Surprisal.head ~ 1 + sibilance * plurality + ", x))
  ) |> unname()
}

# helper: fit a list and return a clean 1-row-per-model data.frame
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

### MODEL SELECTION LOGIC

# Define Random Slopes (factor-coded, correlated)
rs <- c("",
        " + sibilance",
        " + plurality",
        " + sibilance + plurality",
        " + sibilance * plurality")

rs_set  <- sapply(rs, \(x) paste0("(1", x, " | set)")) |> unname()
rs_head <- sapply(rs, \(x) paste0("(1", x, " | Head)")) |> unname()

# Numeric (uncorrelated) options
rs_num <- c(" + sibilance_num",
            " + plurality_num",
            " + sibilance_num + plurality_num",
            " + sibilance_num * plurality_num")

# allow intercept-only, OR numeric slopes with ||
rs_set_uncorr  <- c("(1 | set)",  sapply(rs_num, \(x) paste0("(1", x, " || set)"))  |> unname())
rs_head_uncorr <- c("(1 | Head)", sapply(rs_num, \(x) paste0("(1", x, " || Head)")) |> unname())

# -----------------------------------------------------------------------------------------------------------

### MAIN LOOP

cat("Found", length(file_list), "files to process.\n\n")

winners_list <- list()

for (current_file in file_list) {
  
  cat("Processing:", current_file, "...\n")
  
  # Load data
  dat <- read.csv(current_file, check.names = TRUE)
  
  # -----------------------------------------------------------------------------------------------------------
  
  ### DATA PREPARATION
  
  # 1) set = blocks of 4 rows (as they appear)
  # 2) plurality = within each block: 1st & 3rd = Singular, 2nd & 4th = Plural
  # 3) sibilance = Pair 1 vs Pair 2 from Category
  
  dat <- dat %>%
    mutate(
      set = paste0("set_", ((row_number() - 1) %/% 4) + 1),
      plurality = ifelse((row_number() - 1) %% 2 == 0, "Singular", "Plural"),
      sibilance = ifelse(Category == "Pair 1", "Sibilant", "NonSibilant"),
      sibilance = factor(sibilance, levels = c("NonSibilant", "Sibilant")),
      plurality  = factor(plurality,  levels = c("Singular", "Plural"))
    )
  
  contrasts(dat$sibilance) <- c(-0.5, 0.5)
  contrasts(dat$plurality) <- c(-0.5, 0.5)
  
  dat$sibilance_num <- ifelse(dat$sibilance == "NonSibilant", -0.5, 0.5)
  dat$plurality_num  <- ifelse(dat$plurality  == "Singular", -0.5, 0.5)
  
  # -----------------------------------------------------------------------------------------------------------
  
  ### MODEL FITTING
  
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
  
  winner <- all_results[all_results$convergence == 1, ]
  
  if (nrow(winner) > 0) {
    winner <- winner[which.min(winner$AIC), ]
    winner$File <- current_file
    winners_list[[current_file]] <- winner
  } else {
    cat("  WARNING: No models converged for file:", current_file, "\n")
  }
}

# -----------------------------------------------------------------------------------------------------------

### FINAL OUTPUT

cat("\n--- PROCESSING COMPLETE ---\n\n")

final_table <- do.call(rbind, winners_list)

options(width = 500)
rownames(final_table) <- NULL
final_table <- final_table[, c("File", "formula", "AIC")]

print(final_table, right = FALSE, row.names = FALSE)
