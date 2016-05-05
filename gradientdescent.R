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

# Criando polinomios e dummies para regressoes no train set
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

# Avaliando gradient descent no train set
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
  grad_new <- gradDescent(initial_theta, x, y, alpha = 0.3, niter = 100000, lambda = lamb)
  theta_new <- grad_new$theta
  pred_new <- h(theta_new, x_val)
  mses[aux] <- mse(df$validate[,preco], pred_new)
  lambdas[aux] <- lamb
  aux <- aux + 1
}
View(cbind(mses, lambdas))
# Melhor modelo ate o momento parece ser a regressao 7 com regularizacao (lambda = 1.1)

# Avaliando mse e mae com melhor modelo no test set
grad <- gradDescent(initial_theta, x, y, alpha = 0.3, niter = 100000, lambda = 1.1)
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
