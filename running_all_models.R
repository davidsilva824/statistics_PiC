library(lme4)
library(lmerTest)
library(dplyr)

options(scipen = 9999)
options(digits = 3)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1_10M")

models <- data.frame(
  File = c(
    "results_experiment_1_babyLlama_2_10M.csv",
    "results_experiment_1_babyLlama_10M.csv",
    "results_experiment_1_gpt_2_10M.csv",
    "results_experiment_1_gpt_bert_10M_causal.csv",
    "results_experiment_1_gpt_bert_10M_masked.csv",
    "results_experiment_1_gpt_bert_10M_mixed.csv",
    "results_experiment_1_gpt_wee_large.csv",
    "results_experiment_1_gpt_wee_medium.csv",
    "results_experiment_1_gpt_wee_small.csv",
    "results_experiment_1_MOEP.csv",
    "results_experiment_1_OPT_10M.csv",
    "results_experiment_1_ZLATA.csv"
  ),
  
  formula = c(
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num || set) + (1 + plurality_num || Head)",                 # babyLlama_2_10M
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num || set) + (1 + plurality_num || Head)",                 # babyLlama_10M
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity * plurality | set) + (1 + plurality_num || Head)",          # gpt_2_10M
    "Surprisal.head ~ 1 + regularity * plurality + (1 | set) + (1 + plurality | Head)",                                        # gpt_bert_10M_causal
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num || set) + (1 + plurality | Head)",                      # gpt_bert_10M_masked
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num || set) + (1 + plurality | Head)",                      # gpt_bert_10M_mixed
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity | set) + (1 + plurality | Head)",                           # gpt_wee_large
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num + plurality_num || set) + (1 + plurality_num || Head)", # gpt_wee_medium
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity | set) + (1 + regularity_num + plurality_num || Head)",     # gpt_wee_small
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num || set) + (1 | Head)",                                  # MOEP
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity * plurality | set) + (1 + plurality_num || Head)",          # OPT_10M
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity | set) + (1 + plurality | Head)"                            # ZLATA
  ),
  
  stringsAsFactors = FALSE
)

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
  
  m <- tryCatch(
    withCallingHandlers(
      lmer(as.formula(fml_string), dat, REML = TRUE),
      warning = function(w){
        message("WARNING in FILE: ", file, " | CASE: ", case, " | ", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  
  s <- summary(m)
  list(
    ok=TRUE,
    AIC=as.numeric(AIC(m)),
    convergence=hasConverged(m),
    formula=paste(deparse(formula(m), width.cutoff=500), collapse=" "),
    coefs=as.data.frame(s$coefficients)
  )
}

out_file <- "results_treat_PR_vs_SI_10M.txt"
sink(out_file)
cat("Treatment contrasts: Case PR (Plural+Regular refs) vs Case SI (Singular+Irregular refs)\n")
cat("Generated:", format(Sys.time()), "\n\n")

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
      cat("ERROR:", r$error, "\n\n")
      return()
    }
    cat("AIC:", r$AIC, " | convergence:", r$convergence, "\n")
    cat("Formula used:", r$formula, "\n\n")
    print(round(r$coefs, 4)) 
    cat("\n")
  }
  
  print_block("Case PR: Plural + Regular references", rPR)
  print_block("Case SI: Singular + Irregular references", rSI)
}

sink()
cat("Wrote:", out_file, "\n")

