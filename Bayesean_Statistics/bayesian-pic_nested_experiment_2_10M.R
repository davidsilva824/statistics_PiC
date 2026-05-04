library(lme4)
library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)

setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_berent&pinker/experiment_2_without_stories/10M")

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
  d$set <- paste0("set_", rep(seq_len(nrow(d) / 4), each = 4))
  d
}) |> bind_rows()

dat <- dat %>%
  mutate(
    type = ifelse(grepl("Sibilant", Category), "Sibilant", "Regular"),
    plurality = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    type = factor(type, levels = c("Sibilant", "Regular")),
    plurality = factor(plurality, levels = c("Singular", "Plural"))
  )

contrasts(dat$type) <- c(-0.5, 0.5)
contrasts(dat$plurality) <- c(-0.5, 0.5)

m_freq <- lmer(Surprisal.head ~ 1 + plurality / type +
                 (1 + plurality / type | model) +
                 (1 + plurality / type | set) +
                 (1 + plurality / type | Head),
               data = dat)
summary(m_freq) |> print(cor = F)

ggemmeans(m_freq, c("plurality", "type")) |> plot()

priors_surprisal <-
  prior(normal(10, 5), class = Intercept, lb = 0) +
  prior(normal(0, 1), class = b) +
  prior(exponential(2.5), class = sd, group = model, coef = Intercept) +
  prior(exponential(2.5), class = sd, group = set, coef = Intercept) +
  prior(exponential(2.5), class = sd, group = Head, coef = Intercept) +
  prior(exponential(5), class = sd, group = model) +
  prior(exponential(5), class = sd, group = set) +
  prior(exponential(5), class = sd, group = Head) +
  prior(lkj(2), class = cor) +
  prior(exponential(1), class = sigma)

m <- brm(Surprisal.head ~ 1 + plurality / type +
           (1 + plurality / type | model) +
           (1 + plurality / type | set) +
           (1 + plurality / type | Head),
         data = dat,
         prior = priors_surprisal,
         sample_prior = "yes",
         chains = 4, iter = 12000, warmup = 2000,
         cores = 4,
         backend = "cmdstanr",
         file = "C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/Bayesean_Statistics/surprisal_gaussian_nested_experiment_2_10M")

summary(m)

p <- ggeffects::ggemmeans(m, c("plurality", "type"))
p$x <- factor(p$x, levels = c("Singular", "Plural"))
plot(p)

m_samples <- fixef(m, summary = F)
m_mm <- make_standata(formula(m), m$data)$X
m_cmat <- unique(m_mm)
rownames(m_cmat) <- paste(levels(dat$plurality) |> rep(each = 2), levels(dat$type))
m_preds <- t(m_cmat %*% t(m_samples))

m_cmat
posterior_summary(m_preds)

(hyp_sing <- hypothesis(m, "pluralitySingular:type1 = 0"))
log(1 / hyp_sing$hypothesis$Evid.Ratio)

(hyp_plur <- hypothesis(m, "pluralityPlural:type1 = 0"))
log(1 / hyp_plur$hypothesis$Evid.Ratio)

(hyp_int <- hypothesis(m, "pluralityPlural:type1 - pluralitySingular:type1 = 0"))
log(1 / hyp_int$hypothesis$Evid.Ratio)

(m_eff_sing <- hypothesis(m, "pluralitySingular:type1 > 0",
                          scope = "coef", group = "model", alpha = 0.025)$hypothesis)
(m_eff_plur <- hypothesis(m, "pluralityPlural:type1 > 0",
                          scope = "coef", group = "model", alpha = 0.025)$hypothesis)
(m_eff_int <- hypothesis(m, "pluralityPlural:type1 - pluralitySingular:type1 > 0",
                         scope = "coef", group = "model", alpha = 0.025)$hypothesis)

m_eff_int$panel <- "Interaction"
m_eff_sing$panel <- "Singular effect"
m_eff_plur$panel <- "Plural effect"
m_eff_all <- rbind(m_eff_sing, m_eff_plur, m_eff_int)

m_eff_all$panel <- factor(m_eff_all$panel, levels = c("Singular effect", "Plural effect", "Interaction"))
m_eff_all$Group <- factor(m_eff_all$Group, levels = m_eff_all$Group[order(m_eff_sing$Estimate)])

ggplot(m_eff_all, aes(x = Estimate, y = Group)) +
  geom_pointrange(aes(xmin = CI.Lower, xmax = CI.Upper)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey60") +
  facet_wrap(~ panel, ncol = 3) +
  scale_x_continuous(limits = c(-2.25, 2.25)) +
  theme_minimal()