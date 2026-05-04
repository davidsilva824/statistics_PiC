library(lme4)
library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)


setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_1/10M")

file_list <- list.files(pattern = "\\.csv$", full.names = FALSE)

get_model_name <- function(fn){
  x <- fn
  x <- str_replace(x, "\\.csv$", "")
  x <- str_replace(x, "^results_experiment_\\d+_", "")
  x
}

dat <- lapply(file_list, function(f){
  d <- read.csv(f, check.names = TRUE)
  d$model <- get_model_name(f)
  d$File <- f
  d
}) |> bind_rows()

dat <- dat %>%
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Irregular", "Regular")),
    plurality  = factor(plurality, levels = c("Singular", "Plural"))
  )

contrasts(dat$regularity) <- c(-0.5, 0.5)
contrasts(dat$plurality)  <- c(-0.5, 0.5)

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
  ) %>%
  filter(!is.na(set))

### Running the frequentist model for comparison
m_freq <- lmer(Surprisal.head ~ 1 + regularity * plurality +
                 (1 + regularity * plurality | model) + (1 + regularity * plurality | set) + (1 + regularity * plurality | Head),
               data = dat)
summary(m_freq) |> print(cor = F)


### Priors
priors_surprisal <-
  prior(normal(10, 5), class = Intercept, lb = 0) +
  prior(normal(0, 1), class = b) +
  prior(exponential(2.5), class = sd, group = model, coef = Intercept) +
  prior(exponential(2.5), class = sd, group = set, coef = Intercept) +
  prior(exponential(2.5), class = sd, group = Head, coef = Intercept) +
  prior(exponential(5), class = sd, group = model) +
  prior(exponential(5), class = sd, group = set) +
  prior(exponential(5), class = sd, group = Head) +
  prior(lkj(1), class=cor) +
  prior(exponential(1), class=sigma)

m <- brm(Surprisal.head ~ 1 + regularity * plurality +
           (1 + regularity * plurality | model) +
           (1 + regularity * plurality | set) +
           (1 + regularity * plurality | Head),
         data = dat,
         prior = priors_surprisal,
         sample_prior = "yes",
         chains = 4, iter = 12000, warmup = 2000,
         cores = 4,
         backend = "cmdstanr",
         file = "C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/Bayesean_Statistics/surprisal_gaussian_experiment_1_10M")
summary(m)

p <- ggeffects::ggemmeans(m, c("plurality", "regularity"))
p$x <- factor(p$x, levels = c("Singular", "Plural"))
plot(p)

m_samples <- fixef(m, summary = F)                                                        # Obtain posteriors of each coefficient
m_mm <- make_standata(formula(m), m$data)$X                                               # Model matrix 
m_cmat <- unique(m_mm)                                                                    # Contrasts
rownames(m_cmat) <- paste(levels(dat$regularity) |> rep(each = 2), levels(dat$plurality))
m_preds <- t(m_cmat %*% t(m_samples))                                                     # Cell predictions

m_cmat
posterior_summary(m_preds)

# hypothesis(m, "(Intercept + (0.5 * regularity1) + (0.5 * plurality1) + (0.25 * regularity1:plurality1)) -
#            (Intercept + (0.5 * regularity1) + (-0.5 * plurality1) + (-0.25 * regularity1:plurality1)) = 0")
(hyp_reg <- hypothesis(m, "plurality1 + 0.5 * regularity1:plurality1 = 0"))
log(1 / hyp_reg$hypothesis$Evid.Ratio)

# hypothesis(m, "(Intercept + (-0.5 * regularity1) + (0.5 * plurality1) + (-0.25 * regularity1:plurality1)) -
#            (Intercept + (-0.5 * regularity1) + (-0.5 * plurality1) + (0.25 * regularity1:plurality1)) = 0")
(hyp_irreg <- hypothesis(m, "plurality1 - 0.5 * regularity1:plurality1 = 0"))
log(1 / hyp_irreg$hypothesis$Evid.Ratio)

(hyp_int <- hypothesis(m, "regularity1:plurality1 = 0"))
log(1 / hyp_int$hypothesis$Evid.Ratio)

(m_eff_int <- hypothesis(m, "regularity1:plurality1 > 0",
                     scope = "coef", group = "model", alpha = 0.025)$hypothesis)
(m_eff_reg <- hypothesis(m, "plurality1 + 0.5 * regularity1:plurality1 > 0",
                         scope = "coef", group = "model", alpha = 0.025)$hypothesis)
(m_eff_irreg <- hypothesis(m, "plurality1 - 0.5 * regularity1:plurality1 > 0",
                           scope = "coef", group = "model", alpha = 0.025)$hypothesis)

m_eff_int$panel <- "Interaction"
m_eff_reg$panel <- "Regular effect"
m_eff_irreg$panel <- "Irregular effect"
m_eff_all <- rbind(m_eff_int, m_eff_reg, m_eff_irreg)

m_eff_all$panel <- factor(m_eff_all$panel, levels = c("Interaction", "Regular effect", "Irregular effect"))
m_eff_all$Group <- factor(m_eff_all$Group, levels = m_eff_all$Group[order(m_eff_int$Estimate)])

ggplot(m_eff_all, aes(x = Estimate, y = Group)) +
  geom_pointrange(aes(xmin = CI.Lower, xmax = CI.Upper)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey60") +
  facet_wrap(~ panel, ncol = 3) +
  scale_x_continuous(limits = c(-1.75, 2.25)) +
  theme_minimal()


print(summary(m))
print(plot(p))
print(posterior_summary(m_preds))
print(m_eff_int)
print(m_eff_reg)
print(m_eff_irreg)





