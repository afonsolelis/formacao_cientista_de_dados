#Bootcamp Cientista de Dados - Professor Fernando Amaral
library(shiny)
library(RODBC)

ui <- fluidPage(
   
   titlePanel("Lendo Banco de Dados"),
   tableOutput("Tabela")
 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  conexao <- odbcDriverConnect('driver={SQL Server};server=DESKTOP-UD9RQJ9\\SQLEXPRESS;database=VENDAS;trusted_connection=true')
  
  resultado <- sqlQuery(conexao, " select * from dbo.vendas ")
  
  output$Tabela <- renderTable({resultado})
  odbcClose(conexao)
   
}


shinyApp(ui = ui, server = server)

