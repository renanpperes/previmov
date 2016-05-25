# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, Metrics, caret, neuralnet)

# Carregar dados e regressoes
source("dados.R")

# Separacao dos aptos em train, validate e test (three-way validation)
set.seed(4321)
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(seq(nrow(aptos)), nrow(aptos)*cumsum(c(0,spec)), labels = names(spec)))
df = split(aptos, g)

# Criar variaveis do conjunto train e escalonar dados
dummies <- predict(dummyVars(~ bairro, data = df$train), newdata = df$train)
data <- data.frame(preco = df$train$preco, area = df$train$area, area2 = df$train$area^2, area3 = df$train$area^3, quartos = df$train$quartos, quartos2 = df$train$quartos^2, quartos3 = df$train$quartos^3, suites = df$train$suites, suites2 = df$train$suites^2, suites3 = df$train$suites^3, vagas = df$train$vagas, vagas2 = df$train$vagas^2, vagas3 = df$train$vagas^3, dummies[,2:6])
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

# Formulas a serem utilizadas
formula1 <- preco ~ area + quartos + suites + vagas + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE
formula2 <- preco ~ area + area2 + area3 + quartos + quartos2 + quartos3 + suites + suites2 + suites3 + vagas + vagas2 + vagas3 + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE

# Rodar rede neural com multiplas camadas usando a funcao neuralnet
nn <- neuralnet(formula1, data = scaled, hidden = c(3,2), linear.output = TRUE)
plot(nn)

# Criar variaveis do conjunto validate e escalonar dados
dummies_val <- predict(dummyVars(~ bairro, data = df$validate), newdata = df$validate)
data_val <- data.frame(preco = df$validate$preco, area = df$validate$area, area2 = df$validate$area^2, area3 = df$validate$area^3, quartos = df$validate$quartos, quartos2 = df$validate$quartos^2, quartos3 = df$validate$quartos^3, suites = df$validate$suites, suites2 = df$validate$suites^2, suites3 = df$validate$suites^3, vagas = df$validate$vagas, vagas2 = df$validate$vagas^2, vagas3 = df$validate$vagas^3, dummies_val[,2:6])
scaled_val <- as.data.frame(scale(data_val, center = mins, scale = maxs - mins))

# Avaliar mse e mae no conjunto validate
pr.nn_val <- compute(nn, scaled_val[,c(2,5,8,11,14:18)])
pr.nn_val <- pr.nn_val$net.result*(max(data$preco)-min(data$preco))+min(data$preco)
mse(data_val$preco, pr.nn_val)
mae(data_val$preco, pr.nn_val)
View(cbind(data_val$preco, pr.nn_val))

# Three-way validation com montecarlo
prevs <- list()
mses <- numeric(100)
maes <- numeric(100)
mses_lm <- numeric(100)
maes_lm <- numeric(100)
for (i in 1:100) {
  spec = c(train = .6, test = .2, validate = .2)
  g = sample(cut(seq(nrow(aptos)), nrow(aptos)*cumsum(c(0,spec)), labels = names(spec)))
  df = split(aptos, g)
  dummies <- predict(dummyVars(~ bairro, data = df$train), newdata = df$train)
  data <- data.frame(preco = df$train$preco, area = df$train$area, area2 = df$train$area^2, area3 = df$train$area^3, quartos = df$train$quartos, quartos2 = df$train$quartos^2, quartos3 = df$train$quartos^3, suites = df$train$suites, suites2 = df$train$suites^2, suites3 = df$train$suites^3, vagas = df$train$vagas, vagas2 = df$train$vagas^2, vagas3 = df$train$vagas^3, dummies[,2:6])
  maxs <- apply(data, 2, max)
  mins <- apply(data, 2, min)
  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  nn <- neuralnet(formula1, data = scaled, hidden = c(3,2), linear.output = TRUE)
  dummies_val <- predict(dummyVars(~ bairro, data = df$validate), newdata = df$validate)
  data_val <- data.frame(preco = df$validate$preco, area = df$validate$area, area2 = df$validate$area^2, area3 = df$validate$area^3, quartos = df$validate$quartos, quartos2 = df$validate$quartos^2, quartos3 = df$validate$quartos^3, suites = df$validate$suites, suites2 = df$validate$suites^2, suites3 = df$validate$suites^3, vagas = df$validate$vagas, vagas2 = df$validate$vagas^2, vagas3 = df$validate$vagas^3, dummies_val[,2:6])
  scaled_val <- as.data.frame(scale(data_val, center = mins, scale = maxs - mins))
  pr.nn_val <- compute(nn, scaled_val[,c(2,5,8,11,14:18)])
  pr.nn_val <- pr.nn_val$net.result*(max(data$preco)-min(data$preco))+min(data$preco)
  reg <- lm(formula2, data)
  predreg <- predict(reg, newdata = data_val)
  mses[i] <- mse(data_val$preco, pr.nn_val)
  maes[i] <- mae(data_val$preco, pr.nn_val)
  mses_lm[i] <- mse(data_val$preco, predreg)
  maes_lm[i] <- mae(data_val$preco, predreg)
}
mean(mses)
mean(maes)
mean(mses_lm)
mean(maes_lm)
