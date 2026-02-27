library(lme4)
library(lmerTest)
library(dplyr)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1")

# ---- paste your final choices here (File + full formula) ----
models <- data.frame(
  File = c(
    "results_experiment_1_babyLlama_100M.csv",
    "results_experiment_1_bbunzeck__grapheme-llama.csv",
    "results_experiment_1_babylm__opt-125m-strict-2023.csv",
    "results_experiment_1_gpt_2_100M.csv",
    "results_experiment_1_BabyLM-community__babylm-baseline-100m-gpt-bert-causal-focus.csv",
    "results_experiment_1_BabyLM-community__babylm-baseline-100m-gpt-bert-masked-focus.csv",
    "results_experiment_1_BabyLM-community__babylm-baseline-100m-gpt-bert-mixed.csv",
    "results_experiment_1_colinglab__CLASS_IT-140M.csv",
    "results_experiment_1_phonemetransformers__GPT2-85M-BPE-TXT.csv",
    "results_experiment_1_phonemetransformers__GPT2-85M-CHAR-TXT.csv",
    "results_experiment_1_phonemetransformers__GPT2-85M-CHAR-TXT-SPACELESS.csv"
  ),
  formula = c(
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num ||      set) + (1 + plurality | Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num || set) + (1 + plurality | Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + plurality_num || set) + (1 + plurality | Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 | set) + (1 + plurality | Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity | set) + (1 + plurality | Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity + plurality | set) + (1 + plurality | Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity | set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 | set) + (1 + regularity | Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + plurality_num || set) + (1 + plurality_num || Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + plurality_num || set) + (1 + regularity + plurality | Head)",
    "Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num * plurality_num || set) + (1 + plurality_num || Head)"
  ),
  stringsAsFactors = FALSE
)

hasConverged <- function (mm) {
  if (is.null(unlist(mm@optinfo$conv$lme4))) return(1)
  if (isSingular(mm)) return(0)
  return(-1)
}

prep_data_treatment <- function(dat, plurality_ref = c("Plural","Singular")){
  plurality_ref <- match.arg(plurality_ref)

  dat <- dat %>%
    mutate(
      regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
      plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
      regularity = factor(regularity, levels = c("Regular","Irregular")),
      plurality  = if (plurality_ref == "Plural")
        factor(plurality, levels = c("Plural","Singular"))
      else
        factor(plurality, levels = c("Singular","Plural"))
    )

  # numeric coding consistent with the chosen reference (0 = ref, 1 = other)
  dat$regularity_num <- ifelse(dat$regularity == levels(dat$regularity)[1], 0, 1)
  dat$plurality_num  <- ifelse(dat$plurality  == levels(dat$plurality)[1], 0, 1)

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

  dat
}

run_one <- function(file, fml_string, plurality_ref){
  dat <- read.csv(file, check.names = TRUE)
  dat <- prep_data_treatment(dat, plurality_ref = plurality_ref)

  m <- tryCatch(lmer(as.formula(fml_string), dat, REML = TRUE), error = function(e) e)
  if(inherits(m, "error")){
    return(list(ok=FALSE, error=conditionMessage(m)))
  }

  s <- summary(m)
  list(
    ok=TRUE,
    AIC=as.numeric(AIC(m)),
    convergence=hasConverged(m),
    formula=paste(deparse(formula(m), width.cutoff=500), collapse=" "),
    coefs=as.data.frame(s$coefficients)
  )
}

out_file <- "results_treatment_pluralRef_vs_singularRef.txt"
sink(out_file)
cat("Treatment contrasts; comparing plurality reference levels\n")
cat("Generated:", format(Sys.time()), "\n\n")

for(i in seq_len(nrow(models))){
  file <- models$File[i]
  fml  <- models$formula[i]

  cat("============================================================\n")
  cat("FILE:", file, "\n")
  cat("MODEL:", fml, "\n\n")

  rP <- run_one(file, fml, "Plural")
  rS <- run_one(file, fml, "Singular")

  print_block <- function(tag, r){
    cat("----", tag, "----\n")
    if(!r$ok){
      cat("ERROR:", r$error, "\n\n")
      return()
    }
    cat("AIC:", r$AIC, " | convergence:", r$convergence, "\n")
    cat("Formula used:", r$formula, "\n\n")
    print(r$coefs)
    cat("\n")
  }

  print_block("Plural as reference (plurality levels = Plural, Singular)", rP)
  print_block("Singular as reference (plurality levels = Singular, Plural)", rS)
}

sink()
cat("Wrote:", out_file, "\n")