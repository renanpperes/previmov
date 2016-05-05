# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stargazer)

# Carregar dados
source("dados.R")

# Tabelas de estatisticas
## Alterar tipo Box/Garagem para Conjunto Comercial/Sala do linha preco 415mil
(preco_tipo <- data[,.(
  media = mean(preco),
  minimo = min(preco),
  maximo = max(preco),
  N = length(preco)),
  by = tipo])
(quartos_tipo <- data[quartos != "NA",.(
  media=mean(quartos),
  minimo=min(quartos),
  maximo=max(quartos),
  N=length(quartos)),
  by=tipo])
(suites_tipo <- data[,.(
  media=mean(suites),
  minimo=min(suites),
  maximo=max(suites),
  N=length(suites)),
  by=tipo])
(vagas_tipo <- data[,.(
  media=mean(vagas),
  minimo=min(vagas),
  maximo=max(vagas),
  N=length(vagas)),
  by=tipo])
(area_tipo <- data[,.(
  media=mean(area),
  minimo=min(area),
  maximo=max(area),
  N=length(area)),
  by=tipo])
(preco_bairro <- data[,.(
  media=mean(preco),
  minimo=min(preco),
  maximo=max(preco),
  N=length(preco)),
  by=bairro])

table <- stargazer(preco_bairro, type = "text", summary=FALSE)
