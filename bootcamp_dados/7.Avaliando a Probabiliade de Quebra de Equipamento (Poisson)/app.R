#Bootcamp Cientista de Dados - Professor Fernando Amaral
library(shiny)

ui <- fluidPage(
   
   titlePanel("Probabilidade de Falha em Equipamentos"),
   fluidRow(
     column(6,
            radioButtons("Opcao", label = "Selecione o Cálculo",
                         choices = list("Prob. Exata" = 1, "Menos que" = 2, "Mais que" = 3), 
                         selected = 1)
     ),
     column(6,
            numericInput("Ocorrencia", "Ocorrência Atual:", 2, min = 1, max = 99),
            actionButton("Processar","Processar")
     )
   ),
  
   fluidRow(
     column(12,
          
            plotOutput("Graf")
   
   )
  
))

server <- function(input, output) {
    
  observeEvent(input$Processar, {
    
    #Acidentes por dia
    lamb = input$Ocorrencia 
    
    #tipo 1 = exatos, 2 menos que, 3 mais que
    tipo = input$Opcao
    
    
    inic = lamb-2
    fim = lamb+2
    
    if (tipo==1){ 
      x = dpois(inic:fim,lambda=lamb)  
      tit = "Probabilidades de Ocorrência" 
    }
    if (tipo==2){ x = ppois(inic:fim,lambda=lamb)  
    tit = "Probabilidades de Ocorrência Menor que"
    
    }
    if (tipo==3) { x = ppois(inic:fim,lambda=lamb, lower.tail = F)  
    tit = "Probabilidades de Ocorrência Maior que"
    }
    
    z =as.character(round(x,4)) 
    y =as.character(inic:fim) 
    
    lab = paste(y,"prob.:" , z)
    
      output$Graf <- renderPlot({ 
      barplot(x, names.arg=lab, col=gray.colors(5),main = tit)
      box()
      
    })
    
   })

}

shinyApp(ui = ui, server = server)

