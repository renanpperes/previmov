# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, Metrics, caret)

# Carregar dados e regressoes
source("dados.R")

# Separacao dos aptos em train, validate e test
set.seed(4321)
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(seq(nrow(aptos)), nrow(aptos)*cumsum(c(0,spec)), labels = names(spec)))
df = split(aptos, g)

# Funcao para criar polinomios
mapFeature <- function(x, degree=3) {
    return(sapply(0:degree, function(i) x^i))
}
 
# Hypothesis function
h <- function(theta, x) {
    hyp <- x %*% t(theta)
    return(hyp)
}
 
# Cost Function
J <- function(theta, x, y, lambda=0) {
    m <- length(y)
    r <- theta^2
    r[1] <- 0
    j <- 1/(2*m) * sum((h(theta, x) - y)^2) + lambda*sum(r)
    return(j)
}
 
# Gradient descent optimization
gradDescent <- function(theta, x, y, alpha=0.05, niter=1000, lambda=0) {
    m <- length(y)
    cost <- numeric(niter)
    it <- numeric(niter)
    for (i in 1:niter) {
        tt <- theta
        tt[1] <- 0
        dj <- (1/m) * ((t(h(theta,x)-y) %*% x) + (lambda * tt))
        theta <- theta - alpha * dj
        cost[i] <- J(theta, x, y, lambda = lambda)
        it[i] <- i
   }
    return(list(theta = theta, cost = cost, iter = it))
}


x_area <- mapFeature(df$train[,area], degree = 3)
x_quartos <- mapFeature(df$train[,quartos], degree = 3)
x_suites <- mapFeature(df$train[,suites], degree = 3)
x_vagas <- mapFeature(df$train[,vagas], degree = 3)
x_raw <- data.frame(cbind(x_area, x_quartos[,2:4], x_suites[,2:4], x_vagas[,2:4]))
dummies <- predict(dummyVars(~ bairro, data = df$train), newdata = df$train)
x_raw <- as.matrix(cbind(x_raw, dummies[,2:6])) #Features
y <- df$train[,preco] #Preco

# Feature scaling
x <- x_raw
for (i in 2:ncol(x)) {
  x[,i] <- (x_raw[,i] - mean(x_raw[,i]))/sd(x_raw[,i])
}

# Rodando gradient descent no train set
initial_theta <- matrix(rep(0,ncol(x)), nrow=1)
grad <- gradDescent(initial_theta, x, y, alpha = 0.3, niter = 100000, lambda = 0)
theta <- grad$theta
qplot(grad$iter, grad$cost)
qplot(grad$iter[1:10000], grad$cost[1:10000])
qplot(grad$iter[50000:100000], grad$cost[50000:100000])
qplot(grad$iter[60000:100000], grad$cost[60000:100000])
# Note que com alpha = 0.3 o algoritmo converge com a maior rapidez para
# o modelo 7. Sao necessarias quase 60000 iteracoes para a
# convergencia. Acima de 0.3, o algoritmo passa a ter problemas de
# convergencia.

# Avaliando gradient descent no validate set
x_val_area <- mapFeature(df$validate[,area], degree = 3)
x_val_quartos <- mapFeature(df$validate[,quartos], degree = 3)
x_val_suites <- mapFeature(df$validate[,suites], degree = 3)
x_val_vagas <- mapFeature(df$validate[,vagas], degree = 3)
x_val <- data.frame(cbind(x_val_area, x_val_quartos[,2:4], x_val_suites[,2:4], x_val_vagas[,2:4]))
dummies_val <- predict(dummyVars(~ bairro, data = df$validate), newdata = df$validate)
x_val <- as.matrix(cbind(x_val, dummies_val[,2:6]))
for (i in 2:ncol(x_val)) {
  x_val[,i] <- (x_val[,i] - mean(x_raw[,i]))/sd(x_raw[,i])
}
pred <- h(theta, x_val)
mse(df$validate[,preco], pred)
mae(df$validate[,preco], pred)
View(cbind(pred, df$validate[,preco]))

# Encontrando melhor parametro de regularizacao
mses <- numeric(10)
lambdas <- numeric(10)
aux <- 1
for (lamb in seq(0.6,1.5,0.1)) {
  grad_new <- gradDescent(initial_theta, x, y, alpha = 0.3, niter = 60000, lambda = lamb)
  theta_new <- grad_new$theta
  pred_new <- h(theta_new, x_val)
  mses[aux] <- mse(df$validate[,preco], pred_new)
  lambdas[aux] <- lamb
  aux <- aux + 1
}
View(cbind(mses, lambdas))
# Melhor modelo ate o momento parece ser a regressao 7 com regularizacao (lambda = 1.1)

# Avaliando mse e mae com melhor modelo no test set
grad <- gradDescent(initial_theta, x, y, alpha = 0.3, niter = 60000, lambda = 1.1)
theta <- grad$theta
x_test_area <- mapFeature(df$test[,area], degree = 3)
x_test_quartos <- mapFeature(df$test[,quartos], degree = 3)
x_test_suites <- mapFeature(df$test[,suites], degree = 3)
x_test_vagas <- mapFeature(df$test[,vagas], degree = 3)
x_test <- data.frame(cbind(x_test_area, x_test_quartos[,2:4], x_test_suites[,2:4], x_test_vagas[,2:4]))
dummies_test <- predict(dummyVars(~ bairro, data = df$test), newdata = df$test)
x_test <- as.matrix(cbind(x_test, dummies_test[,2:6]))
for (i in 2:ncol(x_test)) {
  x_test[,i] <- (x_test[,i] - mean(x_raw[,i]))/sd(x_raw[,i])
}
pred <- h(theta, x_test)
mse(df$test[,preco], pred)
mae(df$test[,preco], pred)
View(cbind(pred, df$test[,preco]))

# Podemos melhor o método de validacao e avaliar novamente qual o melhor parametro de regularizacao

# 10-fold cross validation sem repeticao
aptos_rd <- aptos[sample(nrow(aptos)),]
folds <- cut(seq(1,nrow(aptos_rd)), breaks=10, labels=FALSE)
pred_cv <- list()
grads <- list()
thetas <- list()
mse_cv <- vector(mode="list", length=10)
mae_cv <- vector(mode="list", length=10)
lambdas <- numeric(10)
for (i in 1:10) {
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- aptos_rd[testIndexes, ]
  trainData <- aptos_rd[-testIndexes, ]
  x_area <- mapFeature(trainData[,area], degree = 3)
  x_quartos <- mapFeature(trainData[,quartos], degree = 3)
  x_suites <- mapFeature(trainData[,suites], degree = 3)
  x_vagas <- mapFeature(trainData[,vagas], degree = 3)
  x_raw <- data.frame(cbind(x_area, x_quartos[,2:4], x_suites[,2:4], x_vagas[,2:4]))
  dummies <- predict(dummyVars(~ bairro, data = trainData), newdata = trainData)
  x_raw <- as.matrix(cbind(x_raw, dummies[,2:6]))
  y <- trainData[,preco]
  x <- x_raw
  for (k in 2:ncol(x)) {
    x[,k] <- (x_raw[,k] - mean(x_raw[,k]))/sd(x_raw[,k])
  }
  initial_theta <- matrix(rep(0,ncol(x)), nrow=1)
  aux <- 1
  for (lamb in seq(1,10,1)) {
    grads[[aux]] <- gradDescent(initial_theta, x, y, alpha = 0.3, niter = 60000, lambda = lamb)
    thetas[[aux]] <- grads[[aux]]$theta
    lambdas[aux] <- lamb
    aux <- aux + 1
  }
  x_test_area <- mapFeature(testData[,area], degree = 3)
  x_test_quartos <- mapFeature(testData[,quartos], degree = 3)
  x_test_suites <- mapFeature(testData[,suites], degree = 3)
  x_test_vagas <- mapFeature(testData[,vagas], degree = 3)
  x_test <- data.frame(cbind(x_test_area, x_test_quartos[,2:4], x_test_suites[,2:4], x_test_vagas[,2:4]))
  dummies_test <- predict(dummyVars(~ bairro, data = testData), newdata = testData)
  x_test <- as.matrix(cbind(x_test, dummies_test[,2:6]))
  for (k in 2:ncol(x_test)) {
    x_test[,k] <- (x_test[,k] - mean(x_raw[,k]))/sd(x_raw[,k])
  }
  for (j in 1:length(grads)) {
    pred_cv[[j]] <- h(thetas[[j]], x_test)
    mse_cv[[j]][i] <- mse(testData[,preco], pred_cv[[j]])
    mae_cv[[j]][i] <- mae(testData[,preco], pred_cv[[j]])
  }
}
lapply(mse_cv, mean)
lapply(mae_cv, mean)
lambdas

# 10-fold cv usando caret package (stratified samples)
flds <- createFolds(aptos[,preco], k = 10, list = TRUE, returnTrain = FALSE)
pred_cv <- list()
grads <- list()
thetas <- list()
mse_cv <- vector(mode="list", length=10)
mae_cv <- vector(mode="list", length=10)
lambdas <- numeric(10)
aux <- 1
for (fold in flds) {
  testData <- aptos[fold,]
  trainData <- aptos[-fold,]
  x_area <- mapFeature(trainData[,area], degree = 3)
  x_quartos <- mapFeature(trainData[,quartos], degree = 3)
  x_suites <- mapFeature(trainData[,suites], degree = 3)
  x_vagas <- mapFeature(trainData[,vagas], degree = 3)
  x_raw <- data.frame(cbind(x_area, x_quartos[,2:4], x_suites[,2:4], x_vagas[,2:4]))
  dummies <- predict(dummyVars(~ bairro, data = trainData), newdata = trainData)
  x_raw <- as.matrix(cbind(x_raw, dummies[,2:6]))
  y <- trainData[,preco]
  x <- x_raw
  for (k in 2:ncol(x)) {
    x[,k] <- (x_raw[,k] - mean(x_raw[,k]))/sd(x_raw[,k])
  }
  initial_theta <- matrix(rep(0,ncol(x)), nrow=1)
  aux2 <- 1
  for (lamb in seq(1,10,1)) {
    grads[[aux2]] <- gradDescent(initial_theta, x, y, alpha = 0.3, niter = 60000, lambda = lamb)
    thetas[[aux2]] <- grads[[aux2]]$theta
    lambdas[aux2] <- lamb
    aux2 <- aux2 + 1
  }
  x_test_area <- mapFeature(testData[,area], degree = 3)
  x_test_quartos <- mapFeature(testData[,quartos], degree = 3)
  x_test_suites <- mapFeature(testData[,suites], degree = 3)
  x_test_vagas <- mapFeature(testData[,vagas], degree = 3)
  x_test <- data.frame(cbind(x_test_area, x_test_quartos[,2:4], x_test_suites[,2:4], x_test_vagas[,2:4]))
  dummies_test <- predict(dummyVars(~ bairro, data = testData), newdata = testData)
  x_test <- as.matrix(cbind(x_test, dummies_test[,2:6]))
  for (k in 2:ncol(x_test)) {
    x_test[,k] <- (x_test[,k] - mean(x_raw[,k]))/sd(x_raw[,k])
  }
  for (j in 1:length(grads)) {
    pred_cv[[j]] <- h(thetas[[j]], x_test)
    mse_cv[[j]][aux] <- mse(testData[,preco], pred_cv[[j]])
    mae_cv[[j]][aux] <- mae(testData[,preco], pred_cv[[j]])
  }
  aux <- aux + 1
}
lapply(mse_cv, mean)
lapply(mae_cv, mean)
lambdas

# 10-fold 100-repeated cv usando funcao train do pacote caret
x_area <- mapFeature(aptos[,area], degree = 3)
x_quartos <- mapFeature(aptos[,quartos], degree = 3)
x_suites <- mapFeature(aptos[,suites], degree = 3)
x_vagas <- mapFeature(aptos[,vagas], degree = 3)
x_raw <- data.frame(cbind(x_area, x_quartos[,2:4], x_suites[,2:4], x_vagas[,2:4]))
dummies <- predict(dummyVars(~ bairro, data = aptos), newdata = aptos)
x_raw <- as.matrix(cbind(x_raw, dummies[,2:6]))
y <- aptos[,preco]
x <- x_raw
for (i in 2:ncol(x)) {
  x[,i] <- (x_raw[,i] - mean(x_raw[,i]))/sd(x_raw[,i])
}

#fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 100)
#trainfit <- train(x = x, y = y, method = "gradDescent",  trControl = fitControl)
