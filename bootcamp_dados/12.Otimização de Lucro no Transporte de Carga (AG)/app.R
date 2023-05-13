#Bootcamp Cientista de Dados - Professor Fernando Amaral
library(shiny)
library(GA)

itens = 0
z = 0

ui <- fluidPage(
   titlePanel("Otimização de Transporte de Cargas"),
   
   tabsetPanel(
     tabPanel("Dados" ,
              fluidRow(
                column(6,fileInput("arquivo", "Selecione o arquivo:",multiple = FALSE,accept = c(".csv"))),
                column(6,actionButton("Totais","Calcular Totais"))
              ),
              fluidRow(
                column(3,h3(textOutput("TQuantidade"))),
                column(3,h3(textOutput("TPesototal"))),
                column(3,h3(textOutput("TVolumetotal"))),
                column(3,h3(textOutput("TValor")))
               ),
              fluidRow(
                column(12, tableOutput("Dados"))
              )
      ),
     tabPanel("Processamento",
              fluidRow(
                column(3,numericInput("sobrapeso", "Informe a sobra de peso",value = 6000)) ,
                column(3,numericInput("sobravolume", "Informe a sobra de Volume",value = 350)),
                column(3,numericInput("iteracoes", "Informe a quantidade de Iterações",value = 10)),
                column(3,actionButton("Processar","Processar") )
              ),
              fluidRow(
                column(3,h3(textOutput("RQuantidade")  )),
                column(3,h3(textOutput("RPesototal"))),
                column(3,h3(textOutput("RVolumetotal"))),
                column(3,h3(textOutput("RValor")))

              ) ,
              fluidRow(
                column(12, tableOutput("Rfinal"))

              )
              )
   )
)
   
server <- function(input, output) {
  
  observeEvent(input$Totais, {
     
    file1 <- input$arquivo
    itens <<-  read.csv(file1$datapath, sep=";")
    z <<- nrow(itens)
    
    output$Dados <- renderTable({itens})
    output$TQuantidade = renderText({paste0("Quantidade de Itens:",z)})
    output$TPesototal = renderText({  paste0("Peso Total: ", sum(itens$PESO ))  })
    output$TVolumetotal = renderText({ paste0("Volume Total: ", sum(itens$VOLUME ))  })
    output$TValor = renderText({ paste0("Valor Total: ", sum(itens$VALOR ))  })
 
  })
  
  observeEvent(input$Processar, {
    maxvolume =  input$sobravolume 
    maxpeso = input$sobrapeso
    
    f <-function(x)
    {
      valor = 0
      peso = 0
      volume = 0
      
      for (i in 1:z)
      {
  
        if (x[ i ] != 0)
        {
          
          valor = valor + itens[i,3]
          peso = peso +  itens[i,2]
          volume = volume +  itens[i,4]
          
        }
      }
    if ( volume > maxvolume | peso > maxpeso )
        valor = 0
      return(valor)
    }
   
    #algoritmo genetico
    resultado = ga("binary", fitness = f, nBits = z,popSize = 10, maxiter = input$iteracoes)
    result = t(as.data.frame( summary(resultado)$solution))
    
    result = itens[result[,1]==1,]
    
    output$Rfinal <- renderTable({result})
    
    output$RQuantidade = renderText({  paste0("Quantidade Final: ", nrow(result)  )})
    output$RPesototal = renderText({  paste0("Peso Final: ", sum(result$PESO ))  })
    output$RVolumetotal = renderText({  paste0("Volume Final: ", sum(result$VOLUME ))  })
    output$RValor = renderText({  paste0("Valor Total: ", sum(result$VALOR ))  })
    
  }) 
  
}


shinyApp(ui = ui, server = server)

