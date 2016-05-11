# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, Metrics, caret)

# Carregar dados
source("dados.R")

# Separacao dos aptos em train, validate e test
set.seed(4321)
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(seq(nrow(aptos)), nrow(aptos)*cumsum(c(0,spec)), labels = names(spec)))
df = split(aptos, g)

# Regressoes Lineares (three-way validation)

formula1 <- preco ~ area
reg1 <- lm(formula1, df$train)
summary(reg1)
prev1 <- predict(reg1, df$test)
View(cbind(prev1, df$test[,preco]))
prev1_val <- predict(reg1, df$validate)
mse(df$validate[,preco], prev1_val)
mae(df$validate[,preco], prev1_val)

formula2 <- preco ~ area + bairro + quartos + suites + vagas
reg2 <- lm(formula2, df$train)
summary(reg2)
prev2 <- predict(reg2, df$test)
View(cbind(prev2, df$test[,preco]))
prev2_val <- predict(reg2, df$validate)
mse(df$validate[,preco], prev2_val)
mae(df$validate[,preco], prev2_val)

formula3 <- preco ~ area + I(area^2) + bairro + quartos + suites + vagas
reg3 <- lm(formula3, df$train)
summary(reg3)
prev3 <- predict(reg3, df$test)
View(cbind(prev3, df$test[,preco]))
prev3_val <- predict(reg3, df$validate)
mse(df$validate[,preco], prev3_val)
mae(df$validate[,preco], prev3_val)

formula4 <- preco ~ area + I(area^2) + I(area^3) + bairro + quartos + suites + vagas
reg4 <- lm(formula4, df$train)
summary(reg4)
prev4 <- predict(reg4, df$test)
View(cbind(prev4, df$test[,preco]))
prev4_val <- predict(reg4, df$validate)
mse(df$validate[,preco], prev4_val)
mae(df$validate[,preco], prev4_val)

formula5 <- preco ~ area + I(area^2) + I(area^3) + I(area^4) + bairro + quartos + suites + vagas
reg5 <- lm(formula5, df$train)
summary(reg5)
prev5 <- predict(reg5, df$test)
View(cbind(prev5, df$test[,preco]))
prev5_val <- predict(reg5, df$validate)
mse(df$validate[,preco], prev5_val)
mae(df$validate[,preco], prev5_val)

formula6 <- preco ~ area + I(area^2) + I(area^3) + I(area^4) + I(area^5) + bairro + quartos + suites + vagas
reg6 <- lm(formula6, df$train)
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

formula7 <- preco ~ area + I(area^2) + I(area^3) + bairro + quartos + I(quartos^2) + I(quartos^3) + suites + I(suites^2) + I(suites^3) + vagas + I(vagas^2) + I(vagas^3)
reg7 <- lm(formula7, df$train)
summary(reg7)
prev7 <- predict(reg7, df$test)
View(cbind(prev7, df$test[,preco]))
prev7_val <- predict(reg7, df$validate)
mse(df$validate[,preco], prev7_val)
mae(df$validate[,preco], prev7_val)
# A regressão 7 parece a melhor de acordo o criterio mse e mae utilizando three-way validation

# Calcular mse e mae para regressao 7 usando dados do conjunto test
mse(df$test[,preco], prev7)
mae(df$test[,preco], prev7)
View(cbind(prev7, df$test[,preco]))

# Three-way validation com montecarlo
models <- list()
prevs <- list()
prevs_test <- list()
mses <- vector(mode="list", length=7)
maes <- vector(mode="list", length=7)
for (i in 1:1000) {
  spec = c(train = .6, test = .2, validate = .2)
  g = sample(cut(seq(nrow(aptos)), nrow(aptos)*cumsum(c(0,spec)), labels = names(spec)))
  df = split(aptos, g)
  for (j in 1:7) {
    models[[j]] <- lm(get(paste0("formula", j)), df$train)
    prevs[[j]] <- predict(models[[j]], df$validate)
    mses[[j]][i] <- mse(df$validate[,preco], prevs[[j]])
    maes[[j]][i] <- mae(df$validate[,preco], prevs[[j]])
  }
}
lapply(mses, mean)
lapply(maes, mean)

# 10-fold 100-repeated cross validation
mses <- matrix(0, 100, 7)
maes <- matrix(0, 100, 7)
for (k in 1:100) {
  aptos_rd <- aptos[sample(nrow(aptos)),]
  folds <- cut(seq(1,nrow(aptos_rd)), breaks=10, labels=FALSE)
  models <- list()
  pred_cv <- list()
  mse_cv <- vector(mode="list", length=7)
  mae_cv <- vector(mode="list", length=7)
  for (i in 1:10) {
    testIndexes <- which(folds==i, arr.ind=TRUE)
    testData <- aptos_rd[testIndexes, ]
    trainData <- aptos_rd[-testIndexes, ]
    for (j in 1:7) {
      models[[j]] <- lm(get(paste0("formula", j)), trainData)
      pred_cv[[j]] <- predict(models[[j]], testData)
      mse_cv[[j]][i] <- mse(testData[,preco], pred_cv[[j]])
      mae_cv[[j]][i] <- mae(testData[,preco], pred_cv[[j]])
    }
  }
  temp_mse <- lapply(mse_cv, mean)
  temp_mae <- lapply(mae_cv, mean)
  mses[k,] <- unlist(temp_mse)
  maes[k,] <- unlist(temp_mae)
}
apply(mses, 2, mean)
apply(maes, 2, mean)

# 10-fold 100-repeated cv usando createFolds (stratified samples)
mses <- matrix(0, 100, 7)
maes <- matrix(0, 100, 7)
for (k in 1:100) {
  flds <- createFolds(aptos[,preco], k = 10, list = TRUE, returnTrain = FALSE)
  models <- list()
  pred_cv <- list()
  mse_cv <- vector(mode="list", length=7)
  mae_cv <- vector(mode="list", length=7)
  aux <- 1
  for (fold in flds) {
    testData <- aptos[fold,]
    trainData <- aptos[-fold,]
    for (j in 1:7) {
      models[[j]] <- lm(get(paste0("formula", j)), trainData)
      pred_cv[[j]] <- predict(models[[j]], testData)
      mse_cv[[j]][aux] <- mse(testData[,preco], pred_cv[[j]])
      mae_cv[[j]][aux] <- mae(testData[,preco], pred_cv[[j]])
    }
    aux <- aux + 1
  }
  temp_mse <- lapply(mse_cv, mean)
  temp_mae <- lapply(mae_cv, mean)
  mses[k,] <- unlist(temp_mse)
  maes[k,] <- unlist(temp_mae)
}
apply(mses, 2, mean)
apply(maes, 2, mean)

# 10-fold 100-repeated cv usando funcao train do pacote caret
mses <- numeric(7)
trainfit <- list()
for (j in 1:7) {
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 100)
  trainfit[[j]] <- train(get(paste0("formula", j)), data = aptos, method = "lm", trControl = fitControl)
  mses[j] <- trainfit[[j]]$results[2]^2
}
mses
