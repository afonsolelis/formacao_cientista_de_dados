#Bootcamp Cientista de Dados - Professor Fernando Amaral

library(shiny)

dados = read.csv("slr12.csv",sep = ";")
modelo = lm(CusInic ~ FrqAnual, data=dados)

ui <- fluidPage(
  
  titlePanel("Previsão de Custo Inicial para Montar uma Franquia"),

  fluidRow(
    column(4,
           h2("Dados"),
           #tabela com os dados
           tableOutput("Dados")
    ),
    column(8,
           #gráfico com os ddos
           plotOutput("Graf")
    )
  ),
  fluidRow(
    column(6,
           h3("Valor Anual da Franquia:"),
           #valor que quer prever
           numericInput("NovoValor", "Insira Novo Valor:", 1500, min = 1, max = 9999999),
           #botao executar
           actionButton("Processar","Processar")
    ),
    column(6,
           #outuput, resulado da previsão
           h1(textOutput("Resultado"))
    )
  )
)

server <- function(input, output) {
  
  #imprime grafico e linha de ajuste
  output$Graf <- renderPlot({ 
      plot(CusInic ~ FrqAnual, data=dados)
      abline(modelo)
  })

  #imprime dados
  output$Dados <- renderTable({head(dados,10) })
  
  #evento do botao para executar o calculo
  observeEvent(input$Processar, {
    valr =  input$NovoValor 
    prev =  predict(modelo,data.frame(FrqAnual = eval(parse(text = valr)))     )
    prev = paste0("Previsão de Custo Inicial R$: ",round(prev,2))
    output$Resultado = renderText({prev})
  })
}
shinyApp(ui = ui, server = server)

