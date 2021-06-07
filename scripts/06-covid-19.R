library(shiny)
library(tidyverse)
library(brazilmaps)
library(sf)
library(gather.covid)
library(leaflet)
library(plotly)


# para instalar o pacote utilize o comando
# remotes::install_github("predict-icmc/gather-data")
#remotes::install_github("rpradosiqueira/brazilmaps")


df_vacina <- read_csv("vacinados_munic.csv")

map_plt <- df_vacina %>% group_by(municipio, dose) %>%
  summarise(total = sum(n)) %>% pivot_wider(names_from = dose, values_from = total)

df_vacina <- df_vacina %>% pivot_wider(names_from = dose, values_from = n)

# definindo o dataframe que contém dados geométricos
shp <- get_brmap("City")
shp_sf <- st_as_sf(shp)# %>%
  #st_transform(4326)

# remover acentos
shp_sf$nome <- iconv(shp_sf$nome, to = "ASCII//TRANSLIT")

# unindo os dados de COVID-19 com as geometrias dos estados
shp_sf <- shp_sf %>% dplyr::filter(nome %in% map_plt$municipio & State == 35)

shp_sf <- left_join(shp_sf, map_plt, by = c("nome" = "municipio"))

# dados do covid-19 do site brasil.io

#df_covid <- pegaCorona(tipo = "last_cases") %>% filter(state == "SP")

df_covid <- read_csv("dados-covid-sp.csv")

joinY <- df_covid %>% select(city_ibge_code, estimated_population_2019)

new_df <- left_join(shp_sf, joinY, by = c("City" = "city_ibge_code"))

new_df <- new_df %>% mutate(perc1dos = `1a Dose`/estimated_population_2019, perc2dos = `2a Dose`/estimated_population_2019)



ui <- fluidPage(
  #titlePanel("Mapa da Vacinação - SP"),
  leafletOutput("mapa1", height=300),
  plotlyOutput("city")
)

server <- function(input, output, session) {

  output$mapa1 <- renderLeaflet({

    colorData <- new_df$perc1dos
    pal <- colorBin("Blues", colorData)

    new_df %>%
    leaflet() %>%
      addTiles() %>%
       addPolygons(
         smoothFactor = 0.5,
         fillOpacity = 1,
         weight = 0.5,
         fillColor = ~ pal(colorData),
         opacity = 0.8,
         stroke = T,
         highlightOptions = highlightOptions(
           color = "black",
           weight = 2,
           bringToFront = TRUE
         ), popup = ~ paste0(
           sep = " ",
           "<b>", nome, "<b><br>",
           "Populacão: ", estimated_population_2019, "<br>",
           "% 1a Dose: ", scales::percent(perc1dos, 2), "<br>",
           "% 2a Dose: ", scales::percent(perc2dos, 2), "<br>"
         ),
         label = ~nome, layerId = ~nome)  %>%
     addLegend("bottomright",
              title = "% da População Imunizada (1a Dose)",
             pal = pal,
            values = ~colorData,
          opacity = 0.8, layerId = "colorLegend"
    )

  })

  output$city <- renderPlotly({
    selectedCity <- input$mapa1_shape_click
    if(is.null(selectedCity)) return()

    df_vacina %>% filter(municipio == selectedCity$id) %>%
      plot_ly() %>% add_bars(x = ~data, y =~`1a Dose`, name = "1a Dose") %>%
      add_bars(x = ~data, y =~`2a Dose`, name = "2a Dose") %>%
      layout(
             title = paste0(selectedCity, " - SP"),
             xaxis = list(title = "Data"),
             yaxis = list(title = "Total de Imunizações Diárias"))
  })

}

shinyApp(ui, server)
