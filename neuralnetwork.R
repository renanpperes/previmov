# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, Metrics, caret, neuralnet)

# Carregar dados e regressoes
source("dados.R")

# Separacao dos aptos em train, validate e test
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

formula <- preco ~ area + area2 + area3 + quartos + quartos2 + quartos3 + suites + suites2 + suites3 + vagas + vagas2 + vagas3 + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE

nn <- neuralnet(formula, data = scaled, hidden = c(5,3), linear.output = TRUE)
plot(nn)

pr.nn <- nn$net.result[[1]]*(max(data$preco)-min(data$preco))+min(data$preco)
mse(data$preco, pr.nn)
mae(data$preco, pr.nn)
View(cbind(data$preco, pr.nn))

# Criar variaveis do conjunto validate e escalonar dados
dummies_val <- predict(dummyVars(~ bairro, data = df$validate), newdata = df$validate)
data_val <- data.frame(preco = df$validate$preco, area = df$validate$area, area2 = df$validate$area^2, area3 = df$validate$area^3, quartos = df$validate$quartos, quartos2 = df$validate$quartos^2, quartos3 = df$validate$quartos^3, suites = df$validate$suites, suites2 = df$validate$suites^2, suites3 = df$validate$suites^3, vagas = df$validate$vagas, vagas2 = df$validate$vagas^2, vagas3 = df$validate$vagas^3, dummies_val[,2:6])
scaled_val <- as.data.frame(scale(data_val, center = mins, scale = maxs - mins))

# Avaliar mse e mae no conjunto validate
pr.nn_val <- compute(nn, scaled_val[,2:18])
pr.nn_val <- pr.nn_val$net.result*(max(data$preco)-min(data$preco))+min(data$preco)
mse(data_val$preco, pr.nn_val)
mae(data_val$preco, pr.nn_val)
View(cbind(data_val$preco, pr.nn_val))

# Implementar 10-fold cross validation
