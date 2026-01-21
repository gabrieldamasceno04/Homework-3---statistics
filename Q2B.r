
library(ggplot2)
library(palmerpenguins)
library(ggplot2)
data("penguins")
penguins_clean <- na.omit(penguins)

x <- penguins_clean$body_mass_g
y <- penguins_clean$bill_length_mm

x_b <- mean(x)
y_b <- mean(y)

numerador <- sum((x - x_b) * (y - y_b))
denominador <- sum((x - x_b)^2)
#MEu slope
beta1_manual <- numerador / denominador
#Meu intercepto
beta0_manual <- y_b- (beta1_manual * x_b)
cat("Intercepto", beta0_manual, "\n")
cat("Inclinação", beta1_manual, "\n\n")
modelo_lm <- lm(bill_length_mm ~ body_mass_g, data = penguins_clean)
print(coef(modelo_lm))
diferenca <- coef(modelo_lm) - c(beta0_manual, beta1_manual)
cat("\nDiferença:", diferenca, "\n")
ggplot(penguins_clean, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = beta0_manual, slope = beta1_manual,
              color = "red", size = 1.2) +
  labs(
    title = "Regressão Linear: Massa e Bico",
    subtitle = paste0("y = ", round(beta0_manual, 2), " + ", round(beta1_manual, 4), "x"),
    x = "Massa Corporal",
    y = "Comprimento do Bico"
  ) +
  theme_minimal()