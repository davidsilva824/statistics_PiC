library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)



### Sum

m1 <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/Bayesean_Statistics/surprisal_gaussian_experiment_1_10M.rds")

summary(m1)

p1 <- ggeffects::ggemmeans(m1, c("plurality", "regularity"))
p$x <- factor(p$x, levels = c("Singular", "Plural"))
plot(p1)


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


regular_effect <- hypothesis(m1, "plurality1 + 0.5 * regularity1:plurality1 = 0")

hypothesis(m1, "plurality1 + 0.5 * regularity1:plurality1 = 0")

hypothesis(m1, "plurality1 - 0.5 * regularity1:plurality1 = 0")# apoia hipotese nula, mas numa zoan inconclusiva. 

hypothesis(m1, "regularity1:plurality1 = 0") #interaction






#############

### Nested

m2 <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/Bayesean_Statistics/surprisal_gaussian_nested_experiment_1_10M.rds")

summary(m2)


p2 <- ggeffects::ggemmeans(m2, c("plurality", "regularity"))
p$x <- factor(p$x, levels = c("Singular", "Plural"))
plot(p2)


### Forest plot

m_eff_reg <- hypothesis(m2, "regularityRegular:plurality1 > 0",
                        scope = "coef", group = "model", alpha = 0.025)$hypothesis

m_eff_irreg <- hypothesis(m2, "regularityIrregular:plurality1 > 0",
                          scope = "coef", group = "model", alpha = 0.025)$hypothesis

m_eff_int <- hypothesis(m2, "regularityRegular:plurality1 - regularityIrregular:plurality1 > 0",
                        scope = "coef", group = "model", alpha = 0.025)$hypothesis

m_eff_int$panel <- "Interaction"
m_eff_reg$panel <- "Regular effect"
m_eff_irreg$panel <- "Irregular effect"
m_eff_all <- rbind(m_eff_int, m_eff_reg, m_eff_irreg)#### valores

head()

m_eff_all$panel <- factor(m_eff_all$panel, levels = c("Regular effect", "Irregular effect", "Interaction"))
m_eff_all$Group <- factor(m_eff_all$Group, levels = m_eff_all$Group[order(m_eff_reg$Estimate)])

ggplot(m_eff_all, aes(x = Estimate, y = Group)) +
  geom_pointrange(aes(xmin = CI.Lower, xmax = CI.Upper)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey60") +
  facet_wrap(~ panel, ncol = 3) +
  scale_x_continuous(limits = c(-2.25, 2.25)) +
  theme_minimal()

