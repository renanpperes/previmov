# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, Metrics)

# Carregar dados
source("dados.R")

# Separacao dos aptos em train, validate e test
set.seed(4321)
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(seq(nrow(aptos)), nrow(aptos)*cumsum(c(0,spec)), labels = names(spec)))
df = split(aptos, g)

# Regressoes Lineares (three-way validation)

reg1 <- lm(preco ~ area, df$train)
summary(reg1)
prev1 <- predict(reg1, df$test)
View(cbind(prev1, df$test[,preco]))
prev1_val <- predict(reg1, df$validate)
mse(df$validate[,preco], prev1_val)
mae(df$validate[,preco], prev1_val)

reg2 <- lm(preco ~ area + bairro + quartos + suites + vagas, df$train)
summary(reg2)
prev2 <- predict(reg2, df$test)
View(cbind(prev2, df$test[,preco]))
prev2_val <- predict(reg2, df$validate)
mse(df$validate[,preco], prev2_val)
mae(df$validate[,preco], prev2_val)

reg3 <- lm(preco ~ area + I(area^2) + bairro + quartos + suites + vagas, df$train)
summary(reg3)
prev3 <- predict(reg3, df$test)
View(cbind(prev3, df$test[,preco]))
prev3_val <- predict(reg3, df$validate)
mse(df$validate[,preco], prev3_val)
mae(df$validate[,preco], prev3_val)

reg4 <- lm(preco ~ area + I(area^2) + I(area^3) + bairro + quartos + suites + vagas, df$train)
summary(reg4)
prev4 <- predict(reg4, df$test)
View(cbind(prev4, df$test[,preco]))
prev4_val <- predict(reg4, df$validate)
mse(df$validate[,preco], prev4_val)
mae(df$validate[,preco], prev4_val)

reg5 <- lm(preco ~ area + I(area^2) + I(area^3) + I(area^4) + bairro + quartos + suites + vagas, df$train)
summary(reg5)
prev5 <- predict(reg5, df$test)
View(cbind(prev5, df$test[,preco]))
prev5_val <- predict(reg5, df$validate)
mse(df$validate[,preco], prev5_val)
mae(df$validate[,preco], prev5_val)

reg6 <- lm(preco ~ area + I(area^2) + I(area^3) + I(area^4) + I(area^5) + bairro + quartos + suites + vagas, df$train)
summary(reg6)
prev6 <- predict(reg6, df$test)
View(cbind(prev6, df$test[,preco]))
prev6_val <- predict(reg6, df$validate)
mse(df$validate[,preco], prev6_val)
mae(df$validate[,preco], prev6_val)

# Observar se ha relacao nao linear entre quartos, suites e vagas com preco
qplot(df$train[,quartos], df$train[,preco])
qplot(df$train[,suites], df$train[,preco])
qplot(df$train[,vagas], df$train[,preco])

reg7 <- lm(preco ~ area + I(area^2) + I(area^3) + bairro + quartos + I(quartos^2) + I(quartos^3) + suites + I(suites^2) + I(suites^3) + vagas + I(vagas^2) + I(vagas^3), df$train)
summary(reg7)
prev7 <- predict(reg7, df$test)
View(cbind(prev7, df$test[,preco]))
prev7_val <- predict(reg7, df$validate)
mse(df$validate[,preco], prev7_val)
mae(df$validate[,preco], prev7_val)
# A regressão 7 parece a melhor de acordo o criterio mse e mae

# Calcular mse e mae para regressao 7 usando dados do conjunto test
mse(df$test[,preco], prev7)
mae(df$test[,preco], prev7)
View(cbind(prev7, df$test[,preco]))
