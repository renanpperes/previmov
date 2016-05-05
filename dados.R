# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table)

# Ler dados de imoveis
data <- read.csv("imoveis_pel.csv", sep = ";", dec = ",")

# Remocao de outliers na variavel data
data <- data.table(data)
data <- data[area != "NA"]
data[preco == 20210000, preco := 2021000]
data[preco == 8800000, preco := 880000]
data <- data[preco != 10000000]
data <- data[is.na(quartos) | quartos != 111]
data <- data[vagas != 150]
data <- data[vagas != 60]
data <- data[vagas != 30]
data[vagas == 23, vagas := 3]
data <- data[quartos == 17, area := 20000]
data <- data[area != 2]
data <- data[area != 4]
data <- data[area != 5]
data <- data[area != 6]
data <- data[area != 8]
data <- data[(area == 9 & tipo == "Terreno Padrao"), area := 90000]
data <- data[area != 1852]
data[,cidade := "Pelotas"]
data <- data[,cidade := droplevels(cidade)]
data <- data[,preco := preco/1000]
data[,bairro := as.factor(toupper(bairro))]

# Subconjunto dos bairros e tipos principais na variavel tb_principais
tipo_bairro <- data[,.(N=length(preco)), by=.(tipo,bairro)]
tb_principais <- tipo_bairro[N >= 10]
tb_principais <- subset(tb_principais, tipo %in% names(which(table(tipo) >= 3)))

# Apartamentos dos principais bairros na variavel aptos
aptos <- subset(data, tipo == "Apartamento")
n_bairro <- aptos[, length(preco), by = bairro]
princ_bairros <- as.character(n_bairro[V1 > 30, bairro])
aptos <- subset(data, tipo == "Apartamento" & bairro %in% princ_bairros)
aptos$bairro <- aptos[, droplevels(bairro)]
aptos$tipo <- aptos[, droplevels(tipo)]
aptos$endereco <- aptos[, droplevels(endereco)]
