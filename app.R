# app.R

library(shiny)
library(leaflet)

ui <- fluidPage(
  leafletOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addFullscreenControl()
  })
}

shinyApp(ui, server)
