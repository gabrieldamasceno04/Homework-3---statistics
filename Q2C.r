library(palmerpenguins)
library(ggplot2)


penguins_clean <- na.omit(penguins)
modelo_lm <- lm(bill_length_mm ~ body_mass_g, data = penguins_clean)
residuos <- residuals(modelo_lm)
preditos <- predict(modelo_lm)

penguins_clean$residuos <- residuos
penguins_clean$preditos <- preditos

ggplot(penguins_clean, aes(x = preditos, y = residuos)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Gráfico de Resíduos: Bill Length e Body Mass",
    x = "Preditos",
    y = "Resíduos (Erro)"
  ) +
  theme_minimal()


rmse <- sqrt(mean(residuos^2))

sq_total <- sum((penguins_clean$bill_length_mm - mean(penguins_clean$bill_length_mm))^2)
sq_residuos <- sum(residuos^2)
r_squared <- 1 - (sq_residuos / sq_total)

cat("RMSE:", round(rmse, 4), "\n")
cat("R^2:", round(r_squared, 4), "\n")