#Bootcamp Cientista de Dados - Professor Fernando Amaral
library(shiny)
library(forecast)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Sistema de Análise e Previsão de Séries Temporais"),
  fluidRow(
    column(4,
         fileInput("arquivo", "Escolha o arquivo:",multiple = FALSE,accept = c(".csv")),
         helpText("Observação: O arquivo deve conter apenas uma coluna, sem nome no cabeçalho. A Frequência deve ser mensal")
      
      ),
    column(4,
           dateRangeInput('datas',label = "Período da Série: ",format = "mm/yyyy",language="pt",start = "01/01/2000", end="12/31/2013",startview = "year",separator = " até "),
           helpText("Observação: para definir mês e ano, selecione um dia qualquer")
    ),
    
    column(4,
           numericInput("PeridoPrevisao", "Informe quantos meses quer prever:", 12, min = 1, max = 48),
           actionButton("Processar","Processar")
    )
  ),
 fluidRow(
    column(6,
           plotOutput("GrafSerie")
    ),
    column(6,
           plotOutput("GrafHist")
    )
  ),
  fluidRow(
    column(6,
           plotOutput("GrafBox")
    ),4,
    column(6,
           plotOutput("GrafDec")
    )
  ),
  hr(),
  fluidRow(
    column(6,
           plotOutput("GrafPrev")
    ),
    column(2,
           h1(textOutput("llower")),
           tableOutput("lower")
           
    ),
    column(2,
           h1(textOutput("lmean")),
           tableOutput("mean")
           
    ),
    column(2,
           h1(textOutput("lupper")),
           tableOutput("upper")
    )
  )
)

server <- function(input, output) {
  #evento do botao para processar
  observeEvent(input$Processar,
    {
      #objeto de leitura do arquivo
      file1 <- input$arquivo
      data =  read.csv(file1$datapath, header = F)
     
      #extrai meses e anos do intervalo informado
      anoinic = as.integer(substr(input$datas[1], 1, 4))
      mesinic = as.integer(substr(input$datas[1], 6, 7))
      anofim = as.integer(substr(input$datas[2], 1, 4))
      mesfim = as.integer(substr(input$datas[2], 6, 7))
      
      #transforma o arquivo importanto em uma serie temporal
      data = ts(data,start = c(anoinic, mesinic),end = c(anofim, mesfim), frequency = 12)
      
      #rotinas de impressao da serie
      output$GrafSerie = renderPlot({autoplot(data, main = "Série Original")})
      output$GrafHist = renderPlot({hist(data, main  = "Histograma")})
      output$GrafBox <- renderPlot({boxplot(data, main = "Boxplot")})
	   dec =   decompose(data)
      output$GrafDec <- renderPlot({autoplot(dec, main = "Decomposição")})
    
      #cria o modelo usando arima
      modelo = auto.arima(data)
      #varifica o período escolhido
      valr =  input$PeridoPrevisao
      #faz a previsao
      previsao = forecast(modelo,h=valr)
      
      #dados da previsao
      output$lower <- renderTable({previsao$lower})
      output$mean <- renderTable({previsao$mean })
      output$upper <- renderTable({previsao$upper })
      
      
      output$llower = renderText({"Lower"})
      output$lupper = renderText({"Upper"})
      output$lmean = renderText({"Mean"})
	  
	  #grafico da previsao
      output$GrafPrev <- renderPlot({ autoplot(previsao, main="Previsão da Série")  })

  })

}

shinyApp(ui = ui, server = server)

