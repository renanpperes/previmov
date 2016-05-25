# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, Metrics, caret, neuralnet)

# Carregar dados e regressoes
source("dados.R")

# Separar conjuntos train e test
set.seed(4321)
trainIndex <- createDataPartition(aptos$preco, p=.7, list=F)
trainData <- aptos[trainIndex, ]
testData <- aptos[-trainIndex, ]

# Criar variaveis do conjunto train e escalonar dados
dummiesTrain <- predict(dummyVars(~ bairro, data = trainData), newdata = trainData)
dataTrain <- data.frame(preco = trainData$preco, area = trainData$area, area2 = trainData$area^2, area3 = trainData$area^3, quartos = trainData$quartos, quartos2 = trainData$quartos^2, quartos3 = trainData$quartos^3, suites = trainData$suites, suites2 = trainData$suites^2, suites3 = trainData$suites^3, vagas = trainData$vagas, vagas2 = trainData$vagas^2, vagas3 = trainData$vagas^3, dummiesTrain[,2:6])
maxs <- apply(dataTrain, 2, max)
mins <- apply(dataTrain, 2, min)
scaledTrain <- as.data.frame(scale(dataTrain, center = mins, scale = maxs - mins))
adj1 <- (max(dataTrain$preco)-min(dataTrain$preco))
adj2 <- min(dataTrain$preco)

# Funcao para computar MAE utilizando train
maeSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
   out <- mae(data$obs, data$pred)  
   names(out) <- "MAE"
   out
}

# Formulas a serem utilizadas
formula1 <- preco ~ area + quartos + suites + vagas + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE
formula2 <- preco ~ area + area2 + area3 + quartos + quartos2 + quartos3 + suites + suites2 + suites3 + vagas + vagas2 + vagas3 + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE

# Metodo de cross validacao da funcao train
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, summaryFunction = maeSummary, savePredictions = "final")

# Regressao sem polinomios
set.seed(21)
reg.linear <- train(formula1, data = scaledTrain, method = "lm", trControl = fitControl, metric = "MAE", maximize = FALSE)
reg.linear
(mse.reglinear <- mse(reg.linear$pred$obs*adj, reg.linear$pred$pred*adj))
(mae.reglinear <- mae(reg.linear$pred$obs*adj, reg.linear$pred$pred*adj))

# Regressao com polinomios
set.seed(21)
reg.poly <- train(formula2, data = scaledTrain, method = "lm", trControl = fitControl, metric = "MAE", maximize = FALSE)
reg.poly
(mse.regpoly <- mse(reg.poly$pred$obs*adj1+adj2, reg.poly$pred$pred*adj1+adj2))
(mae.regpoly <- mae(reg.poly$pred$obs*adj1+adj2, reg.poly$pred$pred*adj1+adj2))

# Rede neural com nnet
my.grid <- expand.grid(.decay = c(0.003, 0.001, 5e-4), .size = c(2, 3, 4, 5))
set.seed(21)
nnetfit <- train(formula1, data = scaledTrain, method = "nnet", maxit = 100, tuneGrid = my.grid, trace = F, lineout = 1, trControl = fitControl, metric = "MAE", maximize = FALSE)
nnetfit
plot(nnetfit)
(mse.nnet <- mse(nnetfit$pred$obs*adj1+adj2, nnetfit$pred$pred*adj1+adj2))
(mae.nnet <- mae(nnetfit$pred$obs*adj1+adj2, nnetfit$pred$pred*adj1+adj2))

# Rede neural com neuralnet (mais de uma hiden layers)
my.grid2 <- expand.grid(.layer1 = c(2, 3, 4), .layer2 = c(4), .layer3 = c(0))
set.seed(21)
neuralnetfit <- train(formula1, data = scaledTrain, method = "neuralnet", tuneGrid = my.grid2, trControl = fitControl)
neuralnetfit
plot(neuralnetfit)
(mse.neuralnet <- mse(neuralnetfit$pred$obs*adj1+adj2, neuralnetfit$pred$pred*adj1+adj2))
(mae.neuralnet <- mae(neuralnetfit$pred$obs*adj1+adj2, neuralnetfit$pred$pred*adj1+adj2))

# Linear support vector regression
set.seed(21)
svm.linear <- train(formula1, data = scaledTrain, method = "svmLinear", trControl=fitControl, metric = "MAE", maximize = FALSE)
svm.linear
(mse.svmlinear <- mse(svm.linear$pred$obs*adj1+adj2, svm.linear$pred$pred*adj1+adj2))
(mae.svmlinear <- mae(svm.linear$pred$obs*adj1+adj2, svm.linear$pred$pred*adj1+adj2))

# Support vector regression com radial kernel
svmGrid <- expand.grid(sigma= 2^c(-10, -5, 0), C= 2^c(0:5))
set.seed(21)
svm.radial <- train(formula1, data = scaledTrain, method = "svmRadial", trControl=fitControl, metric = "MAE", maximize = FALSE, tuneGrid = svmGrid)
svm.radial
plot(svm.radial)
(mse.svmradial <- mse(svm.radial$pred$obs*adj1+adj2, svm.radial$pred$pred*adj1+adj2))
(mae.svmradial <- mae(svm.radial$pred$obs*adj1+adj2, svm.radial$pred$pred*adj1+adj2))

# Avaliar criterios para diferentes modelos
rValues <- resamples(list(reglinear=reg.linear, regpoly=reg.poly, nnet=nnetfit, neuralnet=neuralnetfit, svrlinear=svm.linear, svrradial=svm.radial))
rValues$values
summary(rValues)
eval <- data.frame(reglinear=c(mse.reglinear, mae.reglinear),
                   regpoly=c(mse.regpoly, mae.regpoly),
                   nnet=c(mse.nnet, mae.nnet),
                   neuralnet=c(mse.neuralnet, mae.neuralnet),
                   svmlinear=c(mse.svmlinear, mae.svmlinear),
                   svmradial=c(mse.svmradial, mae.svmradial))
rownames(eval) <- c("MSE", "MAE")
eval

# Criar variaveis do conjunto test e escalonar dados
dummiesTest <- predict(dummyVars(~ bairro, data = testData), newdata = testData)
dataTest <- data.frame(preco = testData$preco, area = testData$area, area2 = testData$area^2, area3 = testData$area^3, quartos = testData$quartos, quartos2 = testData$quartos^2, quartos3 = testData$quartos^3, suites = testData$suites, suites2 = testData$suites^2, suites3 = testData$suites^3, vagas = testData$vagas, vagas2 = testData$vagas^2, vagas3 = testData$vagas^3, dummiesTest[,2:6])
scaledTest <- as.data.frame(scale(dataTest, center = mins, scale = maxs - mins))

# Avaliar resultado do melhor modelo no conjunto test
data.predict <- predict(svm.radial, newdata = scaledTest)
data.predict <- data.predict*(max(dataTrain$preco)-min(dataTrain$preco))+min(dataTrain$preco)
mse(data.predict, testData$preco)
mae(data.predict, testData$preco)
