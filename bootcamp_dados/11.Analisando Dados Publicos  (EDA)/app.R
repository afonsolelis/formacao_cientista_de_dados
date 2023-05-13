#Bootcamp Cientista de Dados - Professor Fernando Amaral
library(shiny)
library(RColorBrewer)

dados = read.csv("dados.csv",sep = ";")


ui <- fluidPage(
  
  titlePanel("Despesas de Empenho da Rubrica Diárias no País dos Municípios Gaúchos"),

  fluidRow(
    column(2,   
           h3(textOutput("TEmpenho")),
           tableOutput("SEmpenho")
     ),
    column(4,   
           plotOutput("HistEmpenho")
    ),
    column(2,
           h3(textOutput("TPIB")),
           tableOutput("SPIB")
    ),
    column(4,   
           plotOutput("HistPib") 
    )
  ),
  fluidRow(
    column(3,
           plotOutput("BoxEmpenho") 
    ),
    column(3,
           plotOutput("BoxPib") 
           
    ),
    column(6,
           plotOutput("Disp") 
           
    )
  ),
  fluidRow(
    column(4,
           plotOutput("MaioresEmpenhos") 
    ),
    column(4,
           plotOutput("MaioresPibs")
           
    ),
    column(4,
           plotOutput("MaiorProporcao")  
    )
)
)


server <- function(input, output) {
 
  
  #evento do botao para executar o calculo

  
    #RESUMOS
   output$TEmpenho = renderText({"Dados de Empenho"})
   output$SEmpenho <- renderTable({as.array(summary(dados$VALOREMPENHO))})
   output$TPIB = renderText({"Dados do PIB"})
   output$SPIB <- renderTable({as.array(summary(dados$PIB)) })
    
    
  
    #ESTATÍSTICAS GRÁFICAS
    output$HistEmpenho <- renderPlot({
      
      hist(dados$VALOREMPENHO,main="Valores de Empenho", col = brewer.pal(n = 3, name = "Paired"), xlab = "Empenho")
    })
    
    output$HistPib <- renderPlot({
      hist(dados$PIB,main="Valores de PIB", col = brewer.pal(n = 3, name = "Pastel1"), xlab = "PIB")
    })
    
    output$BoxEmpenho <- renderPlot({
      boxplot(dados$VALOREMPENHO, main="Valores de Empenho", col = brewer.pal(n = 3, name = "Paired"), outline = F, horizontal = T )
    })
    
    output$BoxPib <- renderPlot({
      boxplot(dados$PIB ,main="Valores de PIB", col = brewer.pal(n = 3, name = "Pastel1"), outline = F, horizontal = T )
    })
    
    output$Disp <- renderPlot({
     plot(dados$VALOREMPENHO,dados$PIB,main="Empenho VS Pib", xlab = "Empenho", ylab = "PIB", pch=19, col="blue")
    
      
    })
    
    
    
    
    
    
    #MAIORES EMPENHOS
    Mempenho = head(dados[order(-dados$VALOREMPENHO),],10)
    
    output$MaioresEmpenhos <- renderPlot({
      barplot(Mempenho$VALOREMPENHO, col=brewer.pal(n = 10, name = "RdBu"),las=2,main = "Maiores Empenhos", xlab = "Emprenhos")
      legend("topright",legend=Mempenho$MUNICIPIO,col = brewer.pal(n = 10, name = "RdBu"), lty=1:2, cex=0.6, ncol = 2,lwd=4)
      box()
    })
    
    #MAIORES PIBS
    Mpibs =   head(dados[order(-dados$PIB),],10)
    
    output$MaioresPibs <- renderPlot({
      pie(Mpibs$PIB,col=brewer.pal(n = 10, name = "Spectral"),labels = Mpibs$MUNICIPIO,   main = "Maiores PIBs", xlab="PIBs")
      
    })
    
    #RELACAO PIB/EMPENHO
    #CALCULA QUAL % DO PIB FOI EMPENHADO
    dados$PROPORCAO =   dados$VALOREMPENHO  / dados$PIB 
    
    Mprop = head(dados[order(-dados$PROPORCAO),],10)
    output$MaiorProporcao <- renderPlot({
      
      barplot(Mprop$PROPORCAO,col=brewer.pal(n = 10, name = "Set3"),las=2,main = "Maiores Gastos em Proporção ao PIB", xlab="Proporção")
      legend("topright",legend=Mprop$MUNICIPIO,col = brewer.pal(n = 10, name = "Set3"), lty=1:2, cex=0.6, ncol = 2,lwd=4)
      box()
      
    })
    
  
    
 
}
shinyApp(ui = ui, server = server)

