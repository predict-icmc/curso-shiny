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

}

shinyApp(ui, server)
