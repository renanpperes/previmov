shinyServer(function(input, output) {
  output$table <- renderDataTable(preco_bairro)
  }
)
