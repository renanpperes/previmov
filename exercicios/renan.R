## TRABALHO FINAN�AS - SCRIPT 
## RENAN PORN PERES - 16104450 

# 1.Introdu��o
# Foi proposto como trabalho final da 1� parte da cadeira de Finan�as, realizarmos altera��es que melhorassem o 
# erro de previs�o m�dio, neste scripts contem algumas tentativas, explicadas ao decorrer deste arquivo.

setwd("C:/home/previmov")
# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, Metrics, caret)


source("dados_novos.R") #dados salvos na variavel aptos

### TENTATIVA 1-ALTERAR A FORMA DE IDENTIFICA��O DE OUTLIERS 
# Foi utilizada a fun��o GetOutliers ap�s a exclus�o dos im�veis 
#tipo != Apartamentos. N�o encontrou-se melhoras neste caso

### TENTATIVA 2 - ALTERAR A FORMA DE IDENTIFICA��O DE OUTLIERS 2
# Alterou-se a identifica��o dos outliers para uma fun��o 
#lognormal (RAFAEL REALIZOU O PROCEDIMENTO). Melhoras no MAE 
#e pioras MSE, portanto prefiro deixar sem altera��o na 
#identifica��o de outliers

### TENTATIVA 3 - EXCLUIR OS DADOS QUE EST�O DUPLICADOS
#   Foi utilizado o comando a baixo para excluir os dados duplicados, 
#observa-se uma pior estimativa pois o MAE vai para 52.197 no 
#melhor modelo, isto pode ser explicado possivelmente pela redu��o 
#da amostra, onde 363 observa��es eram iguais em todas as suas 
#caracteristicas, podemos tamb�m incorrer no erro, que existe 
#2 apartamentos identicos, mesmo sendo raro quando desconsidera-se
#a venda de im�veis na planta...

##aptos <- aptos[!duplicated(aptos), ]

# Separar conjuntos train e test (nada a alterar)
set.seed(4321)
trainIndex <- createDataPartition(aptos$preco, p=.7, list=F)
trainData <- aptos[trainIndex, ]
testData <- aptos[-trainIndex, ]

### TENTATIVA 4 - COLETAR DADOS DE COBERTURA
#   Foram coletados manualmente dados de cobertura (REALIZADO 
#POR MICHEL E RENAN), encontramos uma pequena melhora nos 
#resultados mas n�o o suficiente para o retorno ser maior do que 
#o custo de obten��o destes dados (por volta de 0.08 de melhora no MAE), 
#foram alterados para isso o script DADOS_NOVOS e acrescentado 
#as vari�vel abaixo.

# Criar variaveis do conjunto train e escalonar dados
dummiesTrain <- predict(dummyVars(~ bairro, data = trainData), newdata = trainData)
dataTrain <- data.frame(preco = trainData$preco, area = trainData$area, area2 = trainData$area^2, area3 = trainData$area^3, quartos = trainData$quartos, quartos2 = trainData$quartos^2, quartos3 = trainData$quartos^3, suites = trainData$suites, suites2 = trainData$suites^2, suites3 = trainData$suites^3, vagas = trainData$vagas, vagas2 = trainData$vagas^2, vagas3 = trainData$vagas^3, dummiesTrain[,2:ncol(dummiesTrain)])
#dataTrain <- data.frame(preco = trainData$preco, cobertura = trainData$cobertura, area = trainData$area, area2 = trainData$area^2, area3 = trainData$area^3, quartos = trainData$quartos, quartos2 = trainData$quartos^2, quartos3 = trainData$quartos^3, suites = trainData$suites, suites2 = trainData$suites^2, suites3 = trainData$suites^3, vagas = trainData$vagas, vagas2 = trainData$vagas^2, vagas3 = trainData$vagas^3, dummiesTrain[,2:ncol(dummiesTrain)])

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

### TENTATIVA 5 - CRIAR VARI�VEIS DE INTERA��O 
#   Foram realizadas algumas intera��es entre vari�veis, exemplo: 
#quartos*bairro.CENTRO, vagas*suites...
#N�o encontramos melhora (MICHEL e RENAN), sugest�o aumentar 
#as infroma��es das observa��es...

# Formulas a serem utilizadas
formula1 <- preco ~ area + quartos + suites + vagas + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE
formula2 <- preco ~ area + area2 + area3 + quartos + quartos2 + quartos3 + suites + suites2 + suites3 + vagas + vagas2 + vagas3 + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE
#formula11 <- preco ~ area + quartos + suites + vagas + cobertura + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE
#formula21 <- preco ~ area + area2 + area3 + quartos + quartos2 + quartos3 + suites + suites2 + suites3 + vagas + vagas2 + vagas3 + cobertura + bairro.CENTRO + bairro.FRAGATA + bairro.PORTO + bairro.TRES.VENDAS + bairro.ZONA.NORTE


### TENTATIVA 6 - MELHORAR A VELOCIDADE DE PROCESSAMENTO
#   Altera��o realizada somente para diminuir o custo de processamento, 
#n�o houve altera��es significantes nos erros m�dios...
#Sugest�o Michel - boot632


# Metodo de cross validacao da funcao train
fitControl <- trainControl(method = "cv", number = 10, repeats = 10, summaryFunction = maeSummary, savePredictions = "final")
#fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, summaryFunction = maeSummary, savePredictions = "final")
#fitControl <- trainControl(method = "boot632", number = 5, repeats = 5, summaryFunction = maeSummary, savePredictions = "final")

# Regressao sem polinomios
set.seed(21)
reg.linear <- train(formula1, data = scaledTrain, method = "lm", trControl = fitControl, metric = "MAE", maximize = FALSE)
reg.linear
(mse.reglinear <- mse(reg.linear$pred$obs*adj1+adj2, reg.linear$pred$pred*adj1+adj2))
(mae.reglinear <- mae(reg.linear$pred$obs*adj1+adj2, reg.linear$pred$pred*adj1+adj2))

# Regressao com polinomios
set.seed(21)
reg.poly <- train(formula2, data = scaledTrain, method = "lm", trControl = fitControl, metric = "MAE", maximize = FALSE)
reg.poly
(mse.regpoly <- mse(reg.poly$pred$obs*adj1+adj2, reg.poly$pred$pred*adj1+adj2))
(mae.regpoly <- mae(reg.poly$pred$obs*adj1+adj2, reg.poly$pred$pred*adj1+adj2))

# Support vector regression com radial kernel e polinomios
svmGrid <- expand.grid(sigma= 2^c(-10, -5, 0), C= 2^c(0:5))
set.seed(21)
svm.radial2 <- train(formula2, data = scaledTrain, method = "svmRadial", trControl=fitControl, metric = "MAE", maximize = FALSE, tuneGrid = svmGrid)
svm.radial2
plot(svm.radial2)
(mse.svmradial2 <- mse(svm.radial2$pred$obs*adj1+adj2, svm.radial2$pred$pred*adj1+adj2))
(mae.svmradial2 <- mae(svm.radial2$pred$obs*adj1+adj2, svm.radial2$pred$pred*adj1+adj2))

# Avaliar criterios para diferentes modelos
rValues <- resamples(list(reglinear=reg.linear, regpoly=reg.poly,  svmradial_poly=svm.radial2))
rValues$values
summary(rValues)
pred_eval <- data.frame(reglinear=c(mse.reglinear, mae.reglinear),
                        regpoly=c(mse.regpoly, mae.regpoly),
                        svmradial_poly=c(mse.svmradial2, mae.svmradial2))
rownames(pred_eval) <- c("MSE", "MAE")
pred_eval

# Criar variaveis do conjunto test e escalonar dados
dummiesTest <- predict(dummyVars(~ bairro, data = testData), newdata = testData)
dataTest <- data.frame(preco = testData$preco, area = testData$area, area2 = testData$area^2, area3 = testData$area^3, quartos = testData$quartos, quartos2 = testData$quartos^2, quartos3 = testData$quartos^3, suites = testData$suites, suites2 = testData$suites^2, suites3 = testData$suites^3, vagas = testData$vagas, vagas2 = testData$vagas^2, vagas3 = testData$vagas^3, dummiesTest[,2:ncol(dummiesTrain)])
#dataTest <- data.frame(preco = testData$preco, cobertura = testData$cobertura, area = testData$area, area2 = testData$area^2, area3 = testData$area^3, quartos = testData$quartos, quartos2 = testData$quartos^2, quartos3 = testData$quartos^3, suites = testData$suites, suites2 = testData$suites^2, suites3 = testData$suites^3, vagas = testData$vagas, vagas2 = testData$vagas^2, vagas3 = testData$vagas^3, dummiesTest[,2:ncol(dummiesTrain)])
scaledTest <- as.data.frame(scale(dataTest, center = mins, scale = maxs - mins))

# Avaliar resultado do melhor modelo no conjunto test
pred_test <- predict(svm.radial2, newdata = scaledTest)
pred_test <- pred_test*adj1+adj2
mse(pred_test, testData$preco)
mae(pred_test, testData$preco)
View(cbind(pred_test, testData$preco))

### Sugest�o: Observar os maiores erros de previsao e ver quais 
#vari�veis tem peso maior, quais caracteristicas s�o mais 
#frequentes para considerar no conjunto teste, aplicar
#possivelmente um modelo de escolha binaria para as 
#observa��es que obtiveram um erro acima de tal valor
#EX: 1 para observa��es com erro > 50 mil e 0 caso contr�rio

#Gerar uma vari�vel levando em conta a probabilidade de ser 1
#dado as caracteristicas destacada no modelo de escolha bin�ria

## Conjunto train
pred_train <- predict(svm.radial2, newdata = scaledTrain)
pred_train <- pred_train*adj1+adj2
mse(pred_train, trainData$preco)
mae(pred_train, trainData$preco)
erro_train <- abs(pred_train - trainData$preco)
errortrain <- cbind(erro_train, pred_train, trainData)
View(errortrain[order(erro_train, decreasing = TRUE),])

## Conjunto test
erro_test <- abs(pred_test - testData$preco)
errortest <- cbind(erro_test, pred_test, testData)
View(errortest[order(erro_test, decreasing = TRUE),])
