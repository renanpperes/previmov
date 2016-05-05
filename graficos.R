# Instalar e carregar pacotes necessarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, plotly)

# Carregar dados
source("dados.R")

# Funcao boxplot
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

# Boxplot com pacote ggplot2
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
  coord_cartesian(ylim=c(0,10)) +
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
  coord_cartesian(ylim=c(0,10)) +
  ggtitle("Preco de imoveis por tipo") +
  xlab("") +
  ylab("Preco por area")

ggplot(subset(data, tipo %in% tb_principais[,unique(tipo)] &
                    bairro %in% tb_principais[,unique(bairro)])) +
  aes(bairro, preco/area) +
  geom_boxplot(aes(fill=bairro)) +
  coord_cartesian(ylim=c(0,10)) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Preco de imoveis por tipo e bairro") +
  xlab("") +
  ylab("Preco por area") +
  facet_wrap(~ tipo)

# Boxplot com funcao qplot
qplot(bairro, preco,
      data=subset(data, bairro %in% names(which(table(bairro) >= 60))),
      geom=c("boxplot"),
      fill=bairro, main="Preco de imoveis por bairro",
      xlab="", ylab="Preco") 

# Boxplot interativo
p <- plot_ly(subset(data, tipo %in% tb_principais[,unique(tipo)] &
                          bairro %in% tb_principais[,unique(bairro)]),
             x = tipo, y = preco/area, color = droplevels(bairro), type = "box") %>%
  layout(title = "Imoveis a venda em Pelotas",
         xaxis = list(title="Tipo do imovel"),
         yaxis = list(title="Preco/m2"), boxmode = "group")
p
## Salvar grafico interativo em html
#htmlwidgets::saveWidget(as.widget(p), "index.html")

# Grafico de area com preco
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

# Criar grafico em nova janela sem sobrescrever a existente
#X11() # Para Linux
#windows() # Para Windows
