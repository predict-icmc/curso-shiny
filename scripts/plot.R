library(shiny)
library(plotly)


df_vacina <- read_csv("vacinados_munic.csv")
df_vacina <- df_vacina %>% pivot_wider(names_from = dose, values_from = n)

cidades <- unique(df_vacina$municipio) %>% sort()

ui <- fluidPage(

  selectInput("city", "escolha a cidade", cidades),
  plotlyOutput("grafico")
)

server <- function(input, output, session) {
  output$grafico <- renderPlotly({

    selectedCity <- input$city

    df_vacina %>% filter(municipio == selectedCity) %>%
      plot_ly() %>% add_bars(x = ~data, y = ~`1a Dose`, name = "1a dose") %>%
      add_bars(x = ~data, y = ~`2a Dose`, name = "2a dose") %>%
      layout(
        title = paste0(selectedCity, " - SP"),
        xaxis = list(title = "Data"),
        yaxis = list(title = "Total de Imunizações Diárias"))
  })
}

shinyApp(ui, server)
