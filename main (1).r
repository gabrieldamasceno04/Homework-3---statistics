# Dados da tabela 1
dados <- c(0.99, 2.31, 10.85, 6.15, 10.81, 3.72, 5.75, 4.15, 9.27, 7.84,
           2.31, 10.85, 6.15, 1.81, 3.72, 5.75, 10.40, 10.04, 4.15, 9.27)

# Dados amostrais
n <- length(dados)       # n = 20
soma_x <- sum(dados)     # soma = 126.29

# Definição da Função de Log-Verossimilhança 
log_verossimilhanca <- function(lambda) {
  return(n * log(lambda) - lambda * soma_x)
}

# MLE
lambda_mle <- n / soma_x  

#valores de lambda
lambdas_seq <- seq(0.05, 0.30, length.out = 200)
l_values <- log_verossimilhanca(lambdas_seq)

plot(lambdas_seq, l_values, type = "l", col = "blue", lwd = 2,
     main = "Funcao Log-Verossimilhanca l(lambda)",
     xlab = expression(lambda),
     ylab = expression(l(lambda)))
abline(v = lambda_mle, col = "red", lty = 2)
points(lambda_mle, log_verossimilhanca(lambda_mle), col = "red", pch = 19)
text(lambda_mle, log_verossimilhanca(lambda_mle), 
     labels = paste("MLE =", round(lambda_mle, 4)), 
     pos = 4, col = "red")

grid()