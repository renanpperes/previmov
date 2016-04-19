# Instalar pacotes necessarios
#install.packages(c("ggplot2", "plotly", "data.table", "stargazer"))

# Carregar pacotes necessarios
library(ggplot2)
library(plotly)
library(data.table)
library(stargazer)

# Ler dados de imoveis
data <- read.csv("imoveis_pel.csv", sep = ";", dec = ",")

# Observar dados e tipo das variaveis
class(data)
str(data)
head(data)
View(data)
dim(data)

# Transformar dados no formato data.table
data <- data.table(data)
class(data)
data

# Lidar com NAs
dim(na.omit(data))
colSums(apply(data, 2, is.na))
data <- data[area != "NA"]

# Deteccao de outliers
names(data)
apply(data[,.(preco,quartos,suites,vagas,area)], 2, max, na.rm = TRUE)
apply(data[,.(preco,quartos,suites,vagas,area)], 2, min, na.rm = TRUE)
## Preco
head(data[order(preco),], 10)
head(data[order(preco, decreasing=TRUE),], 10)
data[preco == 20210000, preco := 2021000]
data[preco == 8800000, preco := 880000]
data <- data[preco != 10000000]
head(data[order(preco, decreasing=TRUE),], 10)
## Quartos
head(data[order(quartos, decreasing=TRUE),], 10)
data <- data[is.na(quartos) | quartos != 111]
head(data[order(quartos, decreasing=TRUE),], 10)
## Suites
head(data[order(suites, decreasing=TRUE),], 10)
## Vagas
head(data[order(vagas, decreasing=TRUE),], 10)
data <- data[vagas != 150]
data <- data[vagas != 60]
data <- data[vagas != 30]
data[vagas == 23, vagas := 3]
head(data[order(vagas, decreasing=TRUE),], 10)
## Area
head(data[order(area),], 10)
data <- data[quartos == 17, area := 20000]
data <- data[area != 2]
data <- data[area != 4]
data <- data[area != 5]
data <- data[area != 6]
data <- data[area != 8]
data <- data[(area == 9 & tipo == "Terreno Padrao"), area := 90000]
head(data[order(area),], 10)
head(data[order(area, decreasing=TRUE),], 10)
head(subset(data, tipo == "Apartamento")[order(area, decreasing=TRUE),], 10)
data <- data[area != 1852]

# Ajustar dados
data[,cidade := "Pelotas"]
data <- data[,cidade := droplevels(cidade)]
data <- data[,preco := preco/1000]
data[,bairro := toupper(bairro)]

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

# Visualizar dados
## Funcao boxplot
boxplot(preco ~ bairro, data)
boxplot(preco ~ droplevels(bairro),
        subset(data, bairro %in% names(which(table(bairro) >= 60))),
        outline=FALSE)
boxplot(preco/area ~ droplevels(bairro),
        subset(data, bairro %in% names(which(table(bairro) >= 60))),
        outline=FALSE)
boxplot(preco ~ droplevels(tipo),
        subset(data, tipo %in% names(which(table(tipo) >= 30))),
        outline=FALSE)
boxplot(preco/area ~ droplevels(tipo),
        subset(data, tipo %in% names(which(table(tipo) >= 30))),
        outline=FALSE)
boxplot(preco ~ quartos, data)
boxplot(preco/area ~ quartos, data)
boxplot(preco ~ suites, data)
boxplot(preco/area ~ suites, data)
boxplot(preco ~ vagas, data)
boxplot(preco/area ~ vagas, data)

## Boxplot com pacote ggplot2
ggplot(subset(data, bairro %in% names(which(table(bairro) >= 60)))) +
  aes(bairro, preco) +
  geom_boxplot(aes(fill=bairro), outlier.shape=NA) +
  coord_cartesian(ylim=c(0,1500)) +
  ggtitle("Preco de imoveis por bairro") +
  xlab("") +
  ylab("Preco")

ggplot(subset(data, bairro %in% names(which(table(bairro) >= 60)))) +
  aes(bairro, preco/area) +
  geom_boxplot(aes(fill=bairro), outlier.shape=NA) +
  coord_cartesian(ylim=c(0,10000)) +
  ggtitle("Preco de imoveis por bairro") +
  xlab("") +
  ylab("Preco por area")

ggplot(subset(data, tipo %in% names(which(table(tipo) >= 30)))) +
  aes(tipo, preco) +
  geom_boxplot(aes(fill=tipo), outlier.shape=NA) +
  coord_cartesian(ylim=c(0,2500)) +
  ggtitle("Preco de imoveis por tipo") +
  xlab("") +
  ylab("Preco")

ggplot(subset(data, tipo %in% names(which(table(tipo) >= 30)))) +
  aes(tipo, preco/area) +
  geom_boxplot(aes(fill=tipo), outlier.shape=NA) +
  coord_cartesian(ylim=c(0,10000)) +
  ggtitle("Preco de imoveis por tipo") +
  xlab("") +
  ylab("Preco por area")

tipo_bairro <- data[,.(N=length(preco)), by=.(tipo,bairro)]
tb_principais <- tipo_bairro[N >= 10]
tb_principais <- subset(tb_principais, tipo %in% names(which(table(tipo) >= 3)))

ggplot(subset(data, tipo %in% tb_principais[,unique(tipo)] &
                    bairro %in% tb_principais[,unique(bairro)])) +
  aes(bairro, preco/area) +
  geom_boxplot(aes(fill=bairro)) +
  coord_cartesian(ylim=c(0,7500)) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Preco de imoveis por tipo e bairro") +
  xlab("") +
  ylab("Preco por area") +
  facet_wrap(~ tipo)

## Boxplot com funcao qplot
qplot(bairro, preco,
      data=subset(data, bairro %in% names(which(table(bairro) >= 60))),
      geom=c("boxplot"),
      fill=bairro, main="Preco de imoveis por bairro",
      xlab="", ylab="Preco") 

## Boxplot interativo
p <- plot_ly(subset(data, tipo %in% tb_principais[,unique(tipo)] &
                          bairro %in% tb_principais[,unique(bairro)]),
             x = tipo, y = preco/area, color = droplevels(bairro), type = "box") %>%
  layout(title = "Imoveis a venda em Pelotas",
         xaxis = list(title="Tipo do imovel"),
         yaxis = list(title="Preco/m2"), boxmode = "group")
p
htmlwidgets::saveWidget(as.widget(p), "index.html")

## Grafico de area com preco
plot(data$area, data$preco)

ggplot(subset(data, tipo == "Apartamento" &
                    bairro %in% names(which(table(bairro) >= 60)))) +
  aes(area, preco) +
  geom_point(aes(color=bairro)) +
  geom_smooth(method="lm")

ggplot(subset(data, tipo == "Apartamento" &
                    bairro %in% names(which(table(bairro) >= 60)))) +
  aes(area, preco) +
  geom_point(aes(color=bairro)) +
  geom_smooth()

ggplot(subset(data, tipo == "Apartamento" &
                    bairro %in% names(which(table(bairro) >= 60)))) +
  aes(area, preco) +
  geom_point(aes(color=bairro)) +
  geom_smooth(aes(color=bairro), method="lm", se=FALSE)

ggplot(subset(data, tipo == "Apartamento" &
                    bairro %in% names(which(table(bairro) >= 60)))) +
  aes(area, preco) +
  geom_point(aes(color=bairro)) +
  geom_smooth(aes(color=bairro), method="lm") +
  facet_wrap(~ bairro)
ggplotly()

X11()
windows()
frame()
