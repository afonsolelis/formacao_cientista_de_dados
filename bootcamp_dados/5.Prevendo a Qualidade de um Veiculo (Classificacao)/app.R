#Bootcamp Cientista de Dados - Professor Fernando Amaral
library(shiny)
library(e1071)

#carrega dados
carros = read.csv("car.data",sep = ",")

#cria o modelo
modelo <- naiveBayes(class ~ . ,carros)

#separa os dados para preencher controles
buying =unique(carros$buying)
maint = unique(carros$maint)
doors = unique(carros$doors)
persons = unique(carros$persons)
lug_boot = unique(carros$lug_boot)
safety = unique(carros$safety)

ui <- fluidPage(
  titlePanel("Previsão de Qualidade de Veículos"),
  fluidRow(
    column(4,
           selectInput("buying","Preço:",choices = buying)
    ),
    column(4,
           selectInput("maint","Manutenção:",choices = maint)
    ),
    column(4,
           selectInput("doors","Portas:",choices = doors)
    )
  ),
  fluidRow(
    column(4,
           selectInput("persons","Capacidade de Passageiros:",choices = persons)
    ),
    column(4,
           selectInput("lug_boot","Porta Malas:",choices = lug_boot)
    ),
    column(4,
           selectInput("safety","Segurança:",choices = safety)  
     )
  ),
  fluidRow(
    column(12,
           actionButton("Processar","Processar"),
           h1(textOutput("Resultado"))
    )
  )
)


server <- function(input, output) {
  observeEvent(input$Processar, {
    novocarro = data.frame("buying"=input$buying,"maint"=input$maint,"doors"=input$doors,"persons"=input$persons,"lug_boot"=input$lug_boot,"safety"=input$safety)
    predicao = predict(modelo, novocarro)
    output$Resultado = renderText({as.character(predicao)})
 })

}
shinyApp(ui = ui, server = server)

