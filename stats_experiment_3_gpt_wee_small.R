library(lme4)
library(lmerTest)
library(dplyr)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_3")
### change this file according to the location of this folder. 

# Load data
dat <- read.csv("results_experiment_3_gpt_wee_small.csv", check.names = TRUE)

# Fix categories and factors.
# Creased two new columns for the conditions Regularity and Plurality. 
dat <- dat %>% 
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular", "Irregular")),
    plurality  = factor(plurality,  levels = c("Singular", "Plural")) 
  )
# Taking the example of the first line: regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular")
# regularity = .... creating/overwriting the column regularity.
# ifelse(test, value_if_TRUE, value_if_FALSE): vectorized “if”.
# grepl("Irregular", Category): checks each row’s Category text; returns TRUE if it contains the substring "Irregular", else FALSE.
# "Irregular", "Regular"  what gets assigned when TRUE,FALSE in the "if", in the column of the regularity. 


contrasts(dat$regularity) <- c(-0.5, 0.5)
contrasts(dat$plurality)  <- c(-0.5, 0.5)

dat$regularity_num <- ifelse(dat$regularity == "Regular", -0.5, 0.5)
dat$plurality_num  <- ifelse(dat$plurality  == "Singular", -0.5, 0.5)
# -----------------------------------------------------------


# Create 'set' and 'pair'. Creates two new columns for the set and pair. 
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
      
      TRUE ~ NA_character_ # for rows that don't match the search above. 
    )
  )
# %in% : Is this value in this list?
# ~ : if the left side is true, assign the right side in the new column. 


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


### Convergence and singularity check. 
hasConverged <- function (mm) {
  if ( !inherits(mm, "merMod")) stop("Error: must pass a lmerMod object")
  if (is.null(unlist(mm@optinfo$conv$lme4))) {
    return(1)   # converged
  } else {
    if (isSingular(mm)) return(0) # singular (boundary)
    return(-1) # did not converge (non-singular)
  }
}

### Fitting each model and obtaining the respective AIC. 
fit_model_safe <- function(fml){
  out <- tryCatch({
    m <- lmer(fml, dat, REML = FALSE)
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
    \(x) as.formula(paste0("Surprisal.head ~ 1 + regularity * plurality + ", x))
  ) |> unname()
}

# helper: fit a list and return a clean 1-row-per-model data.frame
fit_formula_list <- function(formulas, family_label){
  rows <- vector("list", length(formulas))
  for(i in seq_along(formulas)){
    res <- fit_model_safe(formulas[[i]])
    rows[[i]] <- data.frame(
      AIC = res$AIC,
      convergence = res$convergence,
      formula = paste(deparse(formulas[[i]]), collapse = " "),
      family = family_label,
      error = res$error,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# GROUP 1: CORRELATED set (|)  +  CORRELATED Head (|)
re_g1 <- apply(expand.grid(rs_set, rs_head), 1, \(x) paste(x, collapse = " + "))
fr_g1 <- build_formulas(re_g1)
df_g1 <- fit_formula_list(fr_g1, "set_|__Head_|")

# GROUP 2: UNCORRELATED set (||)  +  UNCORRELATED Head (||)
re_g2 <- apply(expand.grid(rs_set_uncorr, rs_head_uncorr), 1, \(x) paste(x, collapse = " + "))
re_g2 <- re_g2[re_g2 != "(1 | set) + (1 | Head)"]  # as you wanted
fr_g2 <- build_formulas(re_g2)
df_g2 <- fit_formula_list(fr_g2, "set_||__Head_||")

# GROUP 3: CORRELATED set (|)  +  UNCORRELATED Head (||)
re_g3 <- apply(expand.grid(rs_set, rs_head_uncorr), 1, \(x) paste(x, collapse = " + "))
re_g3 <- re_g3[re_g3 != "(1 | set) + (1 | Head)"]  # avoid duplicate baseline
fr_g3 <- build_formulas(re_g3)
df_g3 <- fit_formula_list(fr_g3, "set_|__Head_||")


# GROUP 4: UNCORRELATED set (||)  +  CORRELATED Head (|)
re_g4 <- apply(expand.grid(rs_set_uncorr, rs_head), 1, \(x) paste(x, collapse = " + "))
re_g4 <- re_g4[re_g4 != "(1 | set) + (1 | Head)"]  # avoid duplicate baseline
fr_g4 <- build_formulas(re_g4)
df_g4 <- fit_formula_list(fr_g4, "set_||__Head_|")

# FINAL: EXACTLY 97 MODELS (no duplicates), ordered by lowest AIC first, and showing ALL (even errors)

all_results <- rbind(df_g1, df_g2, df_g3, df_g4)

# keep one row per formula (baseline duplication)
all_results <- all_results[!duplicated(all_results$formula), ]


# order: NA AIC (errors) go to bottom
all_results <- all_results[order(is.na(all_results$AIC), all_results$AIC), ]

print(all_results)

####################################################################################

final_model <- lmer(
  Surprisal.head ~ 1 + regularity * plurality + (1 + regularity_num || set) + (1 + regularity_num * plurality_num || Head), 
  data = dat, 
  REML = TRUE
)

summary(final_model)
