library(ggplot2)
library(palmerpenguins)
library(ggplot2)
data("penguins")
penguins_clean <- na.omit(penguins)

ggplot(penguins_clean, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(alpha = 0.7) + geom_smooth(method = "lm", se = FALSE, color = "blue")
  labs(
    title = "Relação entre Massa Corporal e Comprimento do Bico",
    x = "Massa Corporal",
    y = "Comprimento do Bico"
  ) +
  theme_minimal()
