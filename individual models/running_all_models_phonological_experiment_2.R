library(lme4)
library(lmerTest)
library(dplyr)

options(scipen = 9999)
options(digits = 3)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/all_results")

# Read the winning models generated previously in experiment 1
models <- read.csv(
  "C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/all_results/winning_models_results.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Keep only the columns needed here
models <- models[, c("File", "formula")]

# Keep only files that actually exist in the current folder
models$File <- basename(models$File)
models <- models[file.exists(models$File), ]

hasConverged <- function(mm) {
  if (is.null(unlist(mm@optinfo$conv$lme4))) return(1)
  if (isSingular(mm)) return(0)
  return(-1)
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

# CASE 1: Plural + Regular references
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

# CASE 2: Singular + Irregular references
prep_SI <- function(dat){
  dat <- dat %>%
    mutate(
      regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
      plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
      regularity = factor(regularity, levels = c("Irregular","Regular")),
      plurality  = factor(plurality,  levels = c("Singular","Plural"))
    )
  dat$regularity_num <- ifelse(dat$regularity == "Irregular", 0, 1)
  dat$plurality_num  <- ifelse(dat$plurality  == "Singular", 0, 1)
  add_set(dat)
}

run_one <- function(file, fml_string, case = c("PR","SI")){
  case <- match.arg(case)
  dat0 <- read.csv(file, check.names = TRUE)
  dat  <- if(case == "PR") prep_PR(dat0) else prep_SI(dat0)
  
  warning_messages <- character(0)
  
  m <- tryCatch(
    withCallingHandlers(
      lmer(as.formula(fml_string), dat, REML = TRUE),
      warning = function(w){
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  
  if (inherits(m, "error")) {
    return(list(
      ok = FALSE,
      error = conditionMessage(m),
      warnings = warning_messages
    ))
  }
  
  s <- summary(m)
  
  list(
    ok = TRUE,
    AIC = as.numeric(AIC(m)),
    convergence = hasConverged(m),
    formula = paste(deparse(formula(m), width.cutoff = 500), collapse = " "),
    warnings = warning_messages,
    coefs = as.data.frame(s$coefficients)
  )
}

out_file <- "results_treat_PR_vs_SI.txt"
sink(out_file)

cat("Treatment contrasts: Case PR (Plural+Regular refs) vs Case SI (Singular+Irregular refs)\n")
cat("Generated:", format(Sys.time()), "\n\n")
cat("Number of models found:", nrow(models), "\n\n")

for(i in seq_len(nrow(models))){
  file <- models$File[i]
  fml  <- models$formula[i]
  
  cat("============================================================\n")
  cat("FILE:", file, "\n")
  cat("MODEL:", fml, "\n\n")
  
  rPR <- run_one(file, fml, "PR")
  rSI <- run_one(file, fml, "SI")
  
  print_block <- function(tag, r){
    cat("----", tag, "----\n")
    
    if(!r$ok){
      cat("ERROR:", r$error, "\n")
      if(length(r$warnings) > 0){
        cat("Warnings:\n")
        for(w in r$warnings) cat("-", w, "\n")
      }
      cat("\n")
      return()
    }
    
    cat("AIC:", r$AIC, " | convergence:", r$convergence, "\n")
    cat("Formula used:", r$formula, "\n")
    
    if(length(r$warnings) > 0){
      cat("Warnings:\n")
      for(w in r$warnings) cat("-", w, "\n")
    }
    
    cat("\n")
    print(round(r$coefs, 4))
    cat("\n")
  }
  
  print_block("Case PR: Plural + Regular references", rPR)
  print_block("Case SI: Singular + Irregular references", rSI)
}

sink()

cat("Wrote:", out_file, "\n")