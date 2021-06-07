library(shiny)
library(leaflet)
library(tidyverse)

df_covid <- read_csv("dados-covid-sp.csv")


ui <- fluidPage(
    leafletOutput("mapa1", height = 700)
)

server <- function(input, output, session) {

  output$mapa1 <- renderLeaflet({

    colorData <- df_covid$last_available_confirmed_per_100k_inhabitants
    pal <- colorBin("viridis", colorData)

    df_covid %>%
      leaflet() %>%
      addTiles() %>%
    addCircleMarkers(
      ~longitude,
      ~latitude,
      color = ~pal(colorData),
      radius = ~log(estimated_population_2019, 2),
      popup = ~ paste0(
        sep = " ",
        "<b>", city, "<b><br>",
        "<b>Casos confirmados: </b>", last_available_confirmed, "<br>",
        "<b>Casos por 100k habitantes: </b>", last_available_confirmed_per_100k_inhabitants, "<br>",
        "Novos casos: ", new_confirmed, "<br>",
        "Novos óbitos: ", new_deaths, "<br>",
        "Populacão: ", estimated_population_2019, "<br>",
        "Total de Casos Confirmados: ", last_available_confirmed, "<br>",
        "Total de óbitos: ", last_available_deaths, "<br>",
        "Taxa de letalidade: ", last_available_death_rate
      ),
      label = ~city) %>%
      addLegend("bottomright",
                title = "Casos confirmados por<br>100k habitantes",
                pal = pal,
                values = ~last_available_confirmed_per_100k_inhabitants,
                opacity = 0.8, layerId = "colorLegend"
      )
  })
}

shinyApp(ui, server)
