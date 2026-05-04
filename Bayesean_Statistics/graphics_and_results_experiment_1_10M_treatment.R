library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)

m <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/Bayesean_Statistics/surprisal_gaussian_experiment_1_10M_treatment.rds")

summary(m)

### Main effect graphic

p <- ggeffects::ggemmeans(m, c("plurality", "regularity"))
p$x <- factor(p$x, levels = c("Singular", "Plural"))

m_preds <- as.data.frame(p)

pd <- position_dodge(width = 0.3)
y_pad <- 0.05 * (max(m_preds$conf.high) - min(m_preds$conf.low))

p <- ggplot(
  m_preds,
  aes(x = x, y = predicted, colour = group)
) +
  geom_point(size = 3, position = pd) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    linewidth = 0.8,
    position = pd
  ) +
  coord_cartesian(
    ylim = c(min(m_preds$conf.low) - y_pad, max(m_preds$conf.high) + y_pad)
  ) +
  labs(
    x = "Plurality",
    y = "Surprisal",
    colour = "Regularity"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 14, colour = "gray28"),
    axis.title = element_text(size = 19),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 19),
    legend.text = element_text(size = 18)
  )

print(p)


### Interaction graphic per model

(m_eff_int <- hypothesis(m, "regularityIrregular:pluralitySingular > 0",
                         scope = "coef", group = "model", alpha = 0.025)$hypothesis)

(m_eff_reg <- hypothesis(m, "-pluralitySingular > 0",
                         scope = "coef", group = "model", alpha = 0.025)$hypothesis)

(m_eff_irreg <- hypothesis(m, "-(pluralitySingular + regularityIrregular:pluralitySingular) > 0",
                           scope = "coef", group = "model", alpha = 0.025)$hypothesis)

m_eff_int$effect <- "Regularity × plurality interaction"
m_eff_reg$effect <- "Regular plural effect\n(e.g., ducks trader vs. duck trader)"
m_eff_irreg$effect <- "Irregular plural effect\n(e.g., geese trader vs. goose trader)"

plot_dat <- bind_rows(m_eff_int, m_eff_reg, m_eff_irreg) %>%
  mutate(
    effect = factor(
      effect,
      levels = c(
        "Regularity × plurality interaction",
        "Regular plural effect\n(e.g., ducks trader vs. duck trader)",
        "Irregular plural effect\n(e.g., geese trader vs. goose trader)"
      )
    )
  )

interaction_order <- plot_dat %>%
  filter(effect == "Regularity × plurality interaction") %>%
  arrange(Estimate) %>%
  pull(Group)

plot_dat$Group <- factor(plot_dat$Group, levels = interaction_order)

write.csv(plot_dat, "forest_plot_treatment_values_10M.csv", row.names = FALSE)

p <- ggplot(plot_dat, aes(x = Group, y = Estimate)) +
  coord_flip() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = CI.Lower, ymax = CI.Upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue") +
  facet_wrap(~ effect, ncol = 3) +
  labs(
    x = "Model",
    y = "Estimate"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 20, colour = "gray28"),
    strip.text = element_text(size = 19),
    axis.text.y = element_text(size = 15),
    plot.title = element_text(hjust = 0.5)
  )

print(p)

ggsave(
  "forest_plot_treatment_3panels_10M.png",
  plot = p,
  width = 16,
  height = 7,
  dpi = 300
)

hypothesis(m, "pluralitySingular = 0")
hypothesis(m, "pluralitySingular + regularityIrregular:pluralitySingular = 0")
hypothesis(m, "regularityIrregular:pluralitySingular = 0")

