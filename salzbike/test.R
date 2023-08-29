
library(shiny)

# read in data 
trails <- st_read("data/Wegenetz/Wegenetz_withtrips.shp")

# create basemap 
basemap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto dark") %>%
  addProviderTiles("CartoDB.Positron", group = "Carto light") %>%
  addTiles(group = "OSM standard") %>%
  setView(lng = 13.055, lat = 47.8095, zoom = 10) %>% 
  addLayersControl(baseGroups = c("OSM standard", "Carto dark", "Carto light"),
                   overlayGroups = c("hikers", "bikers", "cluster"),
                   options = layersControlOptions(collapsed = FALSE,
                                                  defaultBase = "Carto dark")) %>% 
  addDrawToolbar(position = "bottomright",
                 singleFeature = TRUE, 
                 polylineOptions = FALSE, 
                 circleMarkerOptions = FALSE,
                 circleOptions = FALSE, 
                 markerOptions = FALSE,
                 editOptions = editToolbarOptions(
                   edit = FALSE, remove = TRUE               )) %>% 
  addCircleMarkers(
    data = st_coordinates(st_startpoint(trails$geometry)),
    clusterOptions = markerClusterOptions(), group = "cluster"
  ) 
ui <- fluidPage(
leafletOutput("map")
)


server <- function(input, output) {
  output$map <- renderLeaflet(basemap)
}

# Run the application 
shinyApp(ui = ui, server = server)
