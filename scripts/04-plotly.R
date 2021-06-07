library(shiny)
library(plotly)
library(tidyverse)


df_vacina <- read_csv("vacinados_munic.csv")
df_vacina <- df_vacina %>% pivot_wider(names_from = dose, values_from = n)

cidades <- unique(df_vacina$municipio) %>% sort()

ui <- fluidPage(
  sidebarLayout(

    sidebarPanel(
      selectInput("variavel1", "Escolha a cidade", cidades)

    ),
    mainPanel(
      plotlyOutput("grafico1"),

    )
  )
)

server <- function(input, output, session) {
  output$grafico1 <- renderPlotly({
      cidade <- input$variavel1

      df_vacina %>% filter(municipio == cidade) %>%
        plot_ly() %>% add_bars(x = ~data, y =~`1a Dose`, name = "1a Dose") %>%
        add_bars(x = ~data, y =~`2a Dose`, name = "2a Dose") %>%
        layout(
          title = paste0(cidade, " - SP"),
          xaxis = list(title = "Data"),
          yaxis = list(title = "Total de Imunizações Diárias"))

  })
}

shinyApp(ui, server)
