library(shiny)
library(plotly)
library(tidyverse)

ui <- fluidPage(
  sidebarPanel(
    selectInput("variavel","Selecione a Variável", colnames(mtcars))
  ),
  mainPanel(
  plotlyOutput("grafico"))
)

server <- function(input, output, session) {
  output$grafico <- renderPlotly({
    var <- input$variavel

    mtcars %>% select(var) %>% plot_ly() %>% add_histogram(mtcars[[var]])
  })
}

shinyApp(ui, server)
