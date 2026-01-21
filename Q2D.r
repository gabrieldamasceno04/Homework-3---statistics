library(palmerpenguins)
library(ggplot2)
library(dplyr)


dados_originais <- na.omit(penguins)

# Criar os dados modificados
dados_mod <- dados_originais

dados_mod$body_mass_g[1] <- 15000
dados_mod$bill_length_mm[1] <- 30


modelo_original <- lm(bill_length_mm ~ body_mass_g, data = dados_originais)
modelo_modificado <- lm(bill_length_mm ~ body_mass_g, data = dados_mod)


get_metrics <- function(modelo) {
  s <- summary(modelo)
  rmse <- sqrt(mean(residuals(modelo)^2))
  c(Intercepto = coef(modelo)[1],
    Inclinacao = coef(modelo)[2],
    R2 = s$r.squared,
    RMSE = rmse)
}

metricas_orig <- get_metrics(modelo_original)
metricas_mod <- get_metrics(modelo_modificado)


comparacao <- data.frame(
  Original = metricas_orig,
  Modificado = metricas_mod
)
print(comparacao)


ggplot(dados_mod, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(alpha = 0.5, color = "grey") +
  geom_point(data = dados_mod[1,], color = "red", size = 4) +
  geom_abline(intercept = coef(modelo_original)[1],
              slope = coef(modelo_original)[2],
              color = "blue", size = 1, linetype = "dashed") +

  geom_abline(intercept = coef(modelo_modificado)[1],
              slope = coef(modelo_modificado)[2],
              color = "red", size = 1) +
  labs(
    title = "Impacto do Outlier na Regressão",
    subtitle = "Azul: Original | Vermelho: Com Outlier",
    x = "Massa Corporal (g)",
    y = "Comprimento do Bico (mm)"
  ) +
  theme_minimal()
