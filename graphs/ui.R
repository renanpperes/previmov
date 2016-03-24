# UI for app
shinyUI(pageWithSidebar(
  # title
  headerPanel("Imóveis em Pelotas"),
  # input
  sidebarPanel(
    uiOutput("type"), #depends on dataset (output$type in server.R)
    selectInput("variable_y","Variável y:",
                list(Preco = "preco", Quartos = "quartos",
                     Suites = "suites", Vagas = "vagas", Area = "area")),
    selectInput("variable_x","Variável x:",
                list(Preco = "preco", Quartos = "quartos",
                     Suites = "suites", Vagas = "vagas", Area = "area")),
    selectInput("group","Grupo:",
                list(Bairro = "bairro", Quartos = "quartos",
                     Suites = "suites", Vagas = "vagas",
                     Cidade = "cidade", Tipo = "tipo")),
    selectInput("plot.type","Tipo do gráfico:",
                list(Boxplot = "boxplot", Histograma = "histogram",
                     Densidade = "density", Barras = "bar",
                     Regressao = "regression")),
    checkboxInput("show.points", "Mostrar pontos", FALSE)),	
  # output				
  mainPanel(
    h3(textOutput("caption")), #h3(htmlOutput("caption"))
    uiOutput("plot")) #depends on input
  )
)
