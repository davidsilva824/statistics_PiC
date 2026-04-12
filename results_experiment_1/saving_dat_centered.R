library(lme4)
library(lmerTest)
library(dplyr)
library(stringr)

# this folder contains the results files obtained from study_pic_4: https://github.com/davidsilva824/study_pic_4 
setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/10M")

file_list <- list.files(pattern="\\.csv$", full.names = FALSE)

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

cat("Found", length(file_list), "files to process.\n\n")

dat <- lapply(file_list, function(f){
  d <- read.csv(f, check.names = TRUE)
  d$model <- get_model_name(f)
  d$File <- f
  d
}) |> bind_rows()







# -------------------------------------------------------------------------------------------
### Data preparation (centered contrasts)

dat <- dat %>%
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular", "Irregular")),
    plurality  = factor(plurality,  levels = c("Singular", "Plural")),
    model      = factor(model)
  )

contrasts(dat$regularity) <- c(-0.5, 0.5)
contrasts(dat$plurality)  <- c(-0.5, 0.5)

dat$regularity_num <- ifelse(dat$regularity == "Regular", -0.5, 0.5)
dat$plurality_num  <- ifelse(dat$plurality  == "Singular", -0.5, 0.5)

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

# drop NA sets
dat <- dat %>% filter(!is.na(set))
write.csv(dat, "dat_centered.csv", row.names = FALSE)