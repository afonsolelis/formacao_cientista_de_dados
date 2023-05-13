#Bootcamp Cientista de Dados - Professor Fernando Amaral
library(shiny)
library(forecast)
library(ggplot2)
library(xts)

ui <- fluidPage(
  titlePanel("Benchmark de Séries Temporais"),
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
           actionButton("Processar","Processar")
    )
  ),
  fluidRow(
    column(12,
           plotOutput("GrafPrev")
    ),
  fluidRow(
    column(6,
           h2(textOutput("TMnaive")),
           tableOutput("Mnaive"),
           h2(textOutput("TMmeanf")),
           tableOutput("Mmeanf"),
           h2(textOutput("TMrwf")),
           tableOutput("Mrwf"),
           h2(textOutput("TMholt")),
           tableOutput("Mholt"),
           h2(textOutput("TMhw")),
           tableOutput("Mhw")
     ),
    column(6,
          h2(textOutput("TMhw2")),
          tableOutput("Mhw2"),
          h2(textOutput("TMhw3")),
          tableOutput("Mhw3"),
          h2(textOutput("TMtslm")),
          tableOutput("Mtslm"),
          h2(textOutput("TMarima")),
          tableOutput("Marima"),
          h2(textOutput("TMnnetar")),
          tableOutput("Mnnetar")
          
    )
   
  )
))


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
   
      valr =  24
     
      treino = window(data, start=c(anoinic,mesinic), end=c(anofim-2, mesfim))
      teste = window(data, start=c(anofim-2,mesinic), end=c(anofim, mesfim))
      
      #naive
      Mnaive = naive(treino, h=valr)
	  #média
      Mmeanf = meanf(treino, h=valr)
	  #drift
      Mrwf = rwf(treino, h=valr, drift = T)
	  #holt - pesos
      Mholt = holt(treino, h=valr)
	  #holt winter aditivo
      Mhw = hw(treino,seasonal = "additive", h=valr)
	  #hold winter multiplicativo
      Mhw2 = hw(treino,seasonal = "multiplicative", h=valr)
	  #holt winter multiplicativo amortecido
      Mhw3 = hw(treino,seasonal = "multiplicative", damped = T, phi = 0.9,h=valr)
    
	  #arima
	  Marima = auto.arima(treino)
      Marima = forecast(Marima, h=valr)
    
	  #linear
	  Mtslm = tslm(treino ~ trend, data=treino)
      Mtslm = forecast(Mtslm, h=valr)
     
	  #rede neural
	  Mnnetar = nnetar(treino)
      Mnnetar = forecast(Mnnetar, h=valr)
   
       output$Mnaive <- renderTable({ accuracy(teste,Mnaive$mean)})
       output$TMnaive = renderText({"Naive"}) 
       
       output$Mmeanf <- renderTable({accuracy(teste,Mmeanf$mean)})
       output$TMmeanf = renderText({"Mean"}) 
       
       output$Mrwf <- renderTable({ accuracy(teste,Mrwf$mean)})
       output$TMrwf = renderText({"Drift" }) 
       
       output$Mholt <- renderTable({accuracy(teste,Mholt$mean)})
       output$TMholt = renderText({"Holt" }) 
       
       output$Mhw <- renderTable({accuracy(teste,Mhw$mean)})
       output$TMhw = renderText({"Holt Winter" }) 
       
       output$Mhw2 <- renderTable({accuracy(teste,Mhw2$mean)})
       output$TMhw2 = renderText({"Holt Winter Multiplicativo" }) 
       
       output$Mhw3 <- renderTable({accuracy(teste,Mhw3$mean)})
       output$TMhw3 = renderText({"Hol Winter Multiplicativo c. Drift" }) 
       
       output$Marima <- renderTable({accuracy(teste,Marima$mean)})
       output$TMarima = renderText({"Arima" }) 
       
       output$Mtslm <- renderTable({accuracy(teste,Mtslm$mean)})
       output$TMtslm = renderText({"Linear" }) 
       
       output$Mnnetar <- renderTable({accuracy(teste,Mnnetar$mean)})
       output$TMnnetar = renderText({"RNA" }) 
       

      output$GrafPrev <- renderPlot({
          par(bg = 'gray90')
          plot(data, main = "Forecast Benchmark")
          lines(Mnaive$mean, type="l", pch=22, lty=6,  col="red", lwd=2)
          lines(Mmeanf$mean, type="l", pch=22, lty=5,  col="blue", lwd=2)
          lines(Mrwf$mean,  type="l", pch=22, lty=4, col="green",lwd=2)
          lines(Mholt$mean, type="l", pch=22, lty=3,  col="chocolate1", lwd=2)
          lines(Mhw$mean, type="l", pch=22, lty=2,  col="slateblue4", lwd=2)
          lines(Mhw2$mean, type="l", pch=22, lty=1,  col="purple", lwd=2)
          lines(Mhw3$mean, type="l", pch=22, lty=6,  col="orangered3", lwd=2)
          lines(Marima$mean, type="l", pch=22, lty=5,  col="skyblue4", lwd=2)
          lines(Mtslm$mean, type="l", pch=22, lty=4,  col="cyan",lwd=2)
          lines(Mnnetar$mean,  type="l", pch=22, lty=3, col="salmon",lwd=2)
          legend("bottomleft",legend=c("Naive","Mean","Dri.","Hol.","HwA","HwM","HwMD","Ari.","Lm","Rn"),
                 col = c("red","blue","green","yellow","pink","purple","orange","maroon","cyan","beige"),
                 lty=1:2, cex=0.8, ncol = 2,lwd=4)
      })

  })

}

shinyApp(ui = ui, server = server)

