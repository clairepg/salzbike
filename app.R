
# 0. load libraries ---------------------------------------------------------
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(lwgeom)
#library(httr)
library(ggplot2)
library(htmltools)
library(shinythemes)
library(thematic)
library(RColorBrewer)
library(shinyWidgets)
library(leaflet.extras)
library(shinyjs)
library(feather)
library(htmlwidgets)


#1.0 load statistics data ---------------------------------------------------------------
# use feather instead of read.csv for much faster loading
# need to convert data beforehand to feather type 

hours_bikers <- read_feather("data/Hourlystats_bikers_Bundesland.feather") 
hours_hikers <- read_feather("data/Hourlystats_hikers_Bundesland.feather") 
weekdays_bikers <- read_feather("data/Weekdaystats_bikers_Bundesland.feather") 
weekdays_hikers <- read_feather("data/Weekdaystats_hikers_Bundesland.feather") 

months_bikers <- read_feather("data/Monthlystats_bikers_Bundsland.feather") 
months_hikers <- read_feather("data/Monthlystats_hikers_Bundsland.feather")
# Define the order of the levels
weekday_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
month_levels <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# Convert 'weekday' into an ordered factor
weekdays_bikers$weekday <- factor(weekdays_bikers$weekday, levels = weekday_levels, ordered = TRUE)
weekdays_hikers$weekday <- factor(weekdays_hikers$weekday, levels = weekday_levels, ordered = TRUE)

# Convert 'month' into an ordered factor
months_bikers$month <- factor(months_bikers$month, levels = month_levels, ordered = TRUE)
months_hikers$month <- factor(months_hikers$month, levels = month_levels, ordered = TRUE)


# 1.1 generate overall distribution plots -----------------------------------------
# Plot for overall distribution of hours for bikers

plot_hour_bikers <- ggplot(hours_bikers, aes(x = hour)) +
  geom_bar(stat = "count") +
  ggtitle("Overall Distribution of\nHours for Bikers") +
  theme(plot.title = element_text(size = 10)) # Adjust the size value as per your preference

# Plot for overall distribution of hours for hikers
plot_hour_hikers <- ggplot(hours_hikers, aes(x = hour)) +
  geom_bar(stat = "count") +
  ggtitle("Overall Distribution of\nHours for Hikers") +
  theme(plot.title = element_text(size = 10))

# Plot for overall distribution of weekdays for bikers
plot_weekday_bikers <- ggplot(weekdays_bikers, aes(x = weekday)) +
  geom_bar(stat = "count") +
  ggtitle("Overall Distribution of\nWeekdays for Bikers") +
  theme(plot.title = element_text(size = 10))

# Plot for overall distribution of weekdays for hikers
plot_weekday_hikers <- ggplot(weekdays_hikers, aes(x = weekday)) +
  geom_bar(stat = "count") +
  ggtitle("Overall Distribution of\nWeekdays for Hikers") +
  theme(plot.title = element_text(size = 10))

# Plot for overall distribution of months for bikers
plot_month_bikers <- ggplot(months_bikers, aes(x = month)) +
  geom_bar(stat = "count") +
  ggtitle("Overall Distribution of\n Months for Bikers") +
  theme(plot.title = element_text(size = 10))+
  scale_x_discrete(labels = function(x) substr(month.abb[match(x, month.name)], 1, 1))

# Plot for overall distribution of months for hikers
plot_month_hikers <- ggplot(months_hikers, aes(x = month)) +
  geom_bar(stat = "count") +
  ggtitle("Overall Distribution of\nMonths for Hikers") +
  theme(plot.title = element_text(size = 10)) +
  scale_x_discrete(labels = function(x) substr(month.abb[match(x, month.name)], 1, 1))


# 2. Read in trail data from shapefile------------------------------------------
# trail data filtered spatially to erase road segments in 
# trail data filtered for only segments with recorded activity 
# merged with activity data both hikers and bikers 
# terrain features from 5m dgm 
# code in which Shapefile is preprocessed: create_shapefile.R
trails <- st_read("data/Wegenetz/Wegenetz_withtrips.shp")

#trails <- trails[1:8000, ]

# 3. Create the origin for the Polylines ---------------------------------------
joined_bikers <- trails
joined_hikers <- trails
#clear memory
trails <- NULL

# 5. Color Palettes ------------------------------------------------------------
bluepalette <- colorRampPalette(c("lightblue", "darkblue"))(n = 20)
color_bike <- colorNumeric(
    palette = bluepalette,
    domain = joined_bikers$bikers,
    na.color = "transparent"
  )
redpalette <- colorRampPalette(c("#FFC0CB", "#8B0000"))(n = 20)
color_hike <- colorNumeric(
    palette = redpalette,
    domain = joined_hikers$hikers,
    na.color = "transparent"
  )


basemap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto dark") %>%
  addProviderTiles("CartoDB.Positron", group = "Carto light") %>%
  addTiles(group = "OSM standard") %>%
  setView(lng = 13.055, lat = 47.8095, zoom = 9) %>% 
  addLayersControl(baseGroups = c("OSM standard", "Carto dark", "Carto light"),
                   overlayGroups = c("bikers", "cluster"),
                   options = layersControlOptions(collapsed = FALSE,
                                                  defaultBase = "Carto dark")) %>% 
  addCircleMarkers(
    data = st_coordinates(st_startpoint(joined_bikers$geometry)),
    clusterOptions = markerClusterOptions(), group = "cluster"
  ) %>% 
  addPolylines(data = joined_bikers, group = "bikers") %>%
  hideGroup("bikers") %>%  
  addDrawToolbar(position = "bottomright", 
                 polylineOptions = FALSE, 
                 circleOptions = FALSE, 
                 markerOptions = FALSE, 
                 circleMarkerOptions = FALSE, 
                 editOptions = editToolbarOptions(remove = TRUE)) %>% # Explicitly hide bikers group during initialization
   onRender("function(el, x) {
  var map = this;
  map.on('draw:created', function(e) {
    Shiny.setInputValue('drawn_shape', e.layer.toGeoJSON());
  });
}"
 )            

# UI ---------------------------------------------------------------------------------
ui <- navbarPage("Salzbike",theme = shinytheme("slate"),collapsible = TRUE,
                 useShinyjs(),
                 tags$head(
                   tags$link(rel = "stylesheet", type = "text/css", href = "styling.css"),
                   tags$script(src = "script.js")
                   ),
                 tabPanel("Map", 
                          div(class="outer",
                            leafletOutput("map", width = "100%", height = "100%"),
                          
                            absolutePanel(top = 0, left = 0, class = "panel-default",
                                          width = "auto", height = "100%",
                              textOutput("clicked_segment"),
                              textOutput("Hoehe"),
                              tags$h1("Filters"),
                              sliderInput("km_filter", 
                                          "Filter by Kilometer", 
                                          min = 0, 
                                          max = 500, 
                                          value = c(0, 500), 
                                          step = 5),
                              checkboxInput("km_checkbox", 
                                            "Filter for the top km",
                                            value = FALSE),
                              sliderInput("altitude_range", "Altitude range",
                                          min = 0, max = 3660, value = c(500, 1000), step = 50),
                              checkboxInput("altitude_checkbox", 
                                            "Filter for altitude",
                                            value = FALSE),
                              sliderInput("diff_range", "Height difference range",
                                          min = 0, max = 720, value = c(50, 100), step = 5),
                              checkboxInput("diff_checkbox", 
                                            "Filter for height difference",
                                            value = FALSE),
                              sliderInput("conflict_range", "Conflict index range",
                                          min = 0, max = 100, value = c(0, 100), step = 5),
                              checkboxInput("conflict_checkbox", 
                                            "Filter for conflict index",
                                            value = FALSE),
                              sliderInput("steepness_range", "Grade percentage",
                                          min = 0, max = 45, value = c(0, 45), step = 5),
                              checkboxInput("steepness_checkbox", 
                                            "Filter for trail steepness",
                                            value = FALSE),
                              materialSwitch(inputId = "map_extent", value = FALSE, label = "Filter only map extent"), 
                              actionButton("reset", "Reset drawing filter"),
                              downloadButton("downloadData", "Download Filtered Shapefile")
                            ),
                            absolutePanel(top = 0, left = "20%", class = "plot-panel",
                                          fluidRow( 
                                            column(2, plotOutput("hour_plot_bikers", height = "20%")),
                                            column(2, plotOutput("hour_plot_hikers", height = "20%")),
                                            column(2, plotOutput("weekday_plot_bikers", height = "20%")),
                                            column(2, plotOutput("weekday_plot_hikers", height = "20%")),
                                            column(2, plotOutput("month_plot_bikers", height = "100%")),
                                            column(2, plotOutput("month_plot_hikers", height = "100%"))
                                          )
                            )
                      )
                            
                 ),
                 tabPanel("Info", 
                          HTML("<h3>Data Sources</h3>"),
                          HTML(" <h4>Strava Metro</h4>"),
                          tags$img(src = "strava_logo.png", height = "100px", width = "100px"),
                          HTML("<br> The activity data is from Strava Metro. A service by Strava, a popular platform for athletes to upload their running, cycling, etc. activities. The data used by this application consists of biking data from 2019, 2020, 2021 and 2022. Additionally walking data (includes running, hiking, walking) from 2019,2020 and 2021."),
                          HTML("<h4>OpenStreetMap API</h4>"),
                          tags$img(src = "Openstreetmap_logo.svg.png", height = "100px", width = "100px"),  # Adjust the height and width as necessary
                          HTML("<br> To extract activity data from forest roads, trails and dirt tracks, road surface data from OpenStreetMap was utilized to delete data from asphalt roads from the analyzed data. The road surface data from OpenStreetMap is not 100% accurate so it can happen that data from normal streets is also included in the shown data."),
                          HTML("<h3>Conflict Index</h3>"),
                          HTML(" The conflict index was calculated by normalizing the values of total trips of both the biking and hiking data. Using the normalized data, the conflict index is calculated that returns values from 0 (no conflict) to 100 (maximum conflict). The conflict index is highest when both bikers and hikers have a high number of total trips for a specific edgeUID.
                               Values of the biking and hiking layer show the same conflict index for the same edgeuid. "),        
                          HTML("<h3>How to use this Dashboard</h3>"),
                          HTML("The dashboard allows for multiple filtering of the data. It is possible to apply multiple filters at the same time. But it is to note that when applying multiple filters, the filtering is done in the sequence of applying the filters. A bit special is the toggle filter, when applied the current map extent is used to select the map bounds for the then following filters. To get the full trail network again one just needs to untoggle the switch."),
                          HTML("<h3>Code</h3>"),
                          HTML("Code and input data used to generate this Shiny mapping tool are available on "),
                          tags$a("Github.", href="https://github.com/clairepg/salzbike"),
                           HTML(" <h4>Contact</h4>")     
                 )
                 
)


# Server logic -----------------------------------------------------------------
server <- function(input, output, session) {
  thematic_shiny() # for setting the plots & etc. to the same shiny theme 

  # output base map ------------------------------------------------------------
  output$map <- renderLeaflet(basemap)
# 2. Click Event: add plots ------------------------------------------------------
  click_status <- reactiveVal(0)
  
  # Modify your observeEvent for input$map_shape_click
  observeEvent(input$map_shape_click, {
    if (is.null(input$map_shape_click$id)) {
      click_status(0)  # no valid shape clicked
      print("no segment here")
    } else {
      click_status(1)  # valid shape clicked
      click_id <- input$map_shape_click$id
      print(click_id)
      updateTextInput(session, "clicked_marker", value = click_id)
    }
  })
  # test edgeuid info 
  output$clicked_segment <- renderText({
    req(input$map_shape_click)
    paste("Ausgewaehltes Segment edgeUID: ", input$map_shape_click$id)
  })
  
  # Filter dataframes for distribution of edgeuid 
  selected_hour_bikers <- reactive({
    req(input$map_shape_click)
    selected_data <- filter(hours_bikers, edgeuid == as.integer(input$map_shape_click$id))
    selected_data
  })
  selected_hour_hikers <- reactive({
    req(input$map_shape_click)
    selected_data <- filter(hours_hikers, edgeuid == as.integer(input$map_shape_click$id))
    selected_data
  })
  selected_weekday_bikers <- reactive({
    req(input$map_shape_click)
    selected_data <- filter(weekdays_bikers, edgeuid == as.integer(input$map_shape_click$id))
    selected_data
  })
  
  selected_weekday_hikers <- reactive({
    req(input$map_shape_click)
    selected_data <- filter(weekdays_hikers, edgeuid == as.integer(input$map_shape_click$id))
    selected_data
  })
  selected_month_bikers <- reactive({
    req(input$map_shape_click)
    selected_data <- filter(months_bikers, edgeuid == as.integer(input$map_shape_click$id))
    selected_data
  })
  selected_month_hikers <- reactive({
    req(input$map_shape_click)
    selected_data <- filter(months_hikers, edgeuid == as.integer(input$map_shape_click$id))
    selected_data
  })
  
  
  output$hour_plot_bikers <- renderPlot({
    if (click_status() == 0) {
      plot_hour_bikers
    } else {
      req(selected_hour_bikers())
      # Render the plot for the selected edgeuid
      ggplot(selected_hour_bikers(), aes(x = hour, y = total_trips)) +
        geom_bar(stat = "identity") +
        ggtitle("Biker Hour Plot") +
        theme(plot.title = element_text(size = 10))
    }
  }, height = 200, width = 200)
  
  output$hour_plot_hikers <- renderPlot({
    if (is.null(input$map_shape_click)) {
      plot_hour_hikers
    } else {
      req(selected_hour_hikers())
      # Render the plot for the selected edgeuid
      ggplot(selected_hour_hikers(), aes(x = hour, y = total_trips)) +
        geom_bar(stat = "identity") +
        ggtitle("Hiker Hour Plot") +
        theme(plot.title = element_text(size = 10))
    }
  }, height = 200, width = 200)
  
  output$weekday_plot_bikers <- renderPlot({
    if (is.null(input$map_shape_click)) {
      plot_weekday_bikers
    } else {
      req(selected_weekday_bikers())
      # Render the plot for the selected edgeuid
      ggplot(selected_weekday_bikers(), aes(x = weekday, y = total_trips)) +
        geom_bar(stat = "identity") +
        ggtitle("Biker Weekday Plot") +
        theme(plot.title = element_text(size = 10))
    }
  }, height = 200, width = 200)
  
  output$weekday_plot_hikers <- renderPlot({
    if (is.null(input$map_shape_click)) {
      plot_weekday_hikers
    } else {
      req(selected_weekday_hikers())
      # Render the plot for the selected edgeuid
      ggplot(selected_weekday_hikers(), aes(x = weekday, y = total_trips)) +
        geom_bar(stat = "identity") +
        ggtitle("Hiker Weekday Plot") +
        theme(plot.title = element_text(size = 10))
    }
  }, height = 200, width = 200)
  
  output$month_plot_bikers <- renderPlot({
    if (is.null(input$map_shape_click)) {
      plot_month_bikers
    } else {
      req(selected_month_bikers())
      ggplot(selected_month_bikers(), aes(x = month, y = total_trips)) +
        geom_bar(stat = "identity") +
        ggtitle("Biker Month Plot") +
        theme(plot.title = element_text(size = 10))+
        scale_x_discrete(labels = function(x) substr(month.abb[match(x, month.name)], 1, 1))
    }
  }, height = 200, width = 200)
  
  output$month_plot_hikers <- renderPlot({
    if (is.null(input$map_shape_click)) {
      plot_month_hikers
    } else {
      req(selected_month_hikers())
      # Render the plot for the selected edgeuid
      ggplot(selected_month_hikers(), aes(x = month, y = total_trips)) +
        geom_bar(stat = "identity") +
        ggtitle("Hiker Month Plot") +
        theme(plot.title = element_text(size = 10)) +
        scale_x_discrete(labels = function(x) substr(month.abb[match(x, month.name)], 1, 1))
    }
  }, height = 200, width = 200)
  

# Combined Filters -------------------------------------------------------------

  captured_bounds <- reactiveVal(NULL)
  # To store drawn polygons
  coords_reactive <- reactiveVal()
  # use a reactiveVal for storing the reset state 
  resetState <- reactiveVal(FALSE)
  
  # reset drawing ----------------------------------
  observeEvent(input$reset, {
    session$sendCustomMessage("clearDrawnItems", message = list())
    print("clear shape")
    coords_reactive(NULL)
    resetState(FALSE)
  })

 
  # drawn polygon 
  # gets coordinates from drawn polygon 
  observe({
    drawn_data <- input$drawn_shape
    if (!is.null(drawn_data) && drawn_data$type == "Feature" && !is.null(drawn_data$geometry$coordinates)) {
      coords_reactive(drawn_data$geometry$coordinates)
      resetState(FALSE) # Set reset state to FALSE when a new shape is drawn.
    }
  })

   initial_zoom_level <- 11 # you can adjust this based on your map's initial zoom
   # set prvious zoom to inital zoom at the start so it doesnt load the
  previous_zoom <- reactiveVal(initial_zoom_level)
 
  
  observe({
    current_zoom <- input$map_zoom
    
    if (!is.null(current_zoom)) {
      
      # Get the previous zoom
      prev_zoom <- previous_zoom()
      
      # Check if zoom crosses the threshold
      crosses_threshold <- is.null(prev_zoom) ||
        (prev_zoom <= 10 && current_zoom > 10) || 
        (prev_zoom > 10 && current_zoom <= 10)
      
      if (crosses_threshold) {
        if (current_zoom <= 10 && 
            !input$km_checkbox && 
            !input$altitude_checkbox && 
            !input$diff_checkbox && 
            !input$conflict_checkbox && 
            !input$steepness_checkbox && 
            !input$map_extent && 
            !is.null(coords_reactive)) {
          leafletProxy("map") %>%
            hideGroup("bikers") %>%
            hideGroup("hikers") %>%
            showGroup("cluster")
        } else {
          leafletProxy("map") %>%
            showGroup("bikers") %>%
            showGroup("hikers") %>%
            hideGroup("cluster")
        }
      }
      
      # Update the previous zoom to the current zoom
      previous_zoom(current_zoom)
    }
  })
  # set map extent 
  observeEvent(input$map_extent, {
    if (input$map_extent) {
      captured_bounds(input$map_bounds)
    } else {
      captured_bounds(NULL)
    }
  })
  
  # Compound filters
  combined_filtered_data <- reactive({
    
    data_bikers <- joined_bikers
    data_hikers <- joined_hikers
    drawn_coords <- coords_reactive()
    
    # If no checkboxes are selected, return original datasets immediately
    if (!input$km_checkbox && !input$altitude_checkbox && !input$diff_checkbox && !input$conflict_checkbox  && !input$steepness_checkbox && !input$map_extent && is.null(drawn_coords) && resetState() == TRUE) {
      return(list(bikers = data_bikers, hikers = data_hikers))
    }
    
    # Filtering based on map extent
    if (input$map_extent && !is.null(captured_bounds())) {
      bounds <- captured_bounds()
      
      # Convert bounds to sf polygon
      map_polygon <- st_polygon(list(matrix(c(bounds$west, bounds$south,
                                              bounds$west, bounds$north,
                                              bounds$east, bounds$north,
                                              bounds$east, bounds$south,
                                              bounds$west, bounds$south), ncol = 2, byrow = TRUE))) %>%
        st_sfc() %>%
        st_set_crs(4326)
      
      # Filter polylines based on intersection with the polygon
      data_bikers <- data_bikers %>% filter(st_intersects(geometry, map_polygon, sparse = FALSE))
      data_hikers <- data_hikers %>% filter(st_intersects(geometry, map_polygon, sparse = FALSE))
    }
   
    if (!is.null(drawn_coords)) {
      # Convert the coordinates into an sf polygon
      drawn_polygon <- st_polygon(list(matrix(unlist(drawn_coords), ncol = 2, byrow = TRUE))) %>%
        st_sfc() %>%
        st_set_crs(4326)
      
      # Filter polylines based on intersection with the drawn polygon
      data_bikers <- data_bikers %>% filter(st_intersects(geometry, drawn_polygon, sparse = FALSE))
      data_hikers <- data_hikers %>% filter(st_intersects(geometry, drawn_polygon, sparse = FALSE))
    }
  
    # Apply km filter
    if (input$km_checkbox) {
      km_filter <- input$km_filter
      data_bikers <- data_bikers %>% 
        arrange(desc(bikers)) %>% 
        mutate(cumulative_km = cumsum(m)) %>% 
        filter(cumulative_km <= km_filter[2])
      data_hikers <- data_hikers %>% 
        arrange(desc(hikers)) %>% 
        mutate(cumulative_km = cumsum(m)) %>% 
        filter(cumulative_km <= km_filter[2])
    }
    
    # Apply altitude filter
    if (input$altitude_checkbox) {
      altitude_range <- input$altitude_range
      data_bikers <- data_bikers %>% filter(Z_Max >= altitude_range[1] & Z_Max <= altitude_range[2])
      data_hikers <- data_hikers %>% filter(Z_Max >= altitude_range[1] & Z_Max <= altitude_range[2])
    }
    
    # Apply height difference filter
    if (input$diff_checkbox) {
      diff_range <- input$diff_range
      data_bikers <- data_bikers %>% filter(h_diff >= diff_range[1] & h_diff <= diff_range[2])
      data_hikers <- data_hikers %>% filter(h_diff >= diff_range[1] & h_diff <= diff_range[2])
    }
    # Apply conflict index filter
    if (input$conflict_checkbox) {
      conflict_range <- input$conflict_range
      data_bikers <- data_bikers %>% filter(conflict >= conflict_range[1] & conflict <= conflict_range[2])
      data_hikers <- data_hikers %>% filter(conflict >= conflict_range[1] & conflict <= conflict_range[2])
    }
    # Apply grade percentage filter
    if (input$steepness_checkbox) {
      steepness_range <- input$steepness_range
      data_bikers <- data_bikers %>% filter(grade >= steepness_range[1] & grade <= steepness_range[2])
      data_hikers <- data_hikers %>% filter(grade >= steepness_range[1] & grade <= steepness_range[2])
    }
   
    return(list(bikers = data_bikers, hikers = data_hikers))
  })
  

  # define as reactive value for download later
  filtered_bikers <- reactiveVal()
  
# observe block applying filters

  observe({

    filtered_bikers_data <- combined_filtered_data()$bikers
    filtered_hikers_data <- combined_filtered_data()$hikers
    print(str(filtered_hikers_data))
    
    #extra line for download data
    filtered_bikers(combined_filtered_data()$bikers)
    
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        hideGroup("cluster") %>% 
        addPolylines(
          group = "bikers",
          data = filtered_bikers_data,
          color = ~color_bike(bikers),
          opacity = 0.8,
          layerId = ~edgeuid,
          popup = ~paste("Edgeuid: ", as.character(edgeuid), "<br>",
                         "Gesamtanzahl Wanderungen: ", as.character(hikers), "<br>",
                         "Gesamtanzahl Radfahrten: ", as.character(bikers), "<br>",  "Segment Laenge: ", as.character(round(m)), " m<br>",
                         "Höchster Punkt: ", as.character(round(Z_Max)), " m ü.M.<br>",
                         "Niedrigster Punkt: ", as.character(round(Z_Min)), " m ü.M.<br>",
                         "Höhendifferenz: ", as.character(round(h_diff)), " m <br>",
                         "Konflikt Index ", as.character(round(conflict)) 
          ),
          highlightOptions = highlightOptions(color = "yellow", weight = 6)
        ) %>%
        addPolylines(
          group = "hikers",
          data = filtered_hikers_data,
          color = ~color_hike(hikers),
          opacity = 0.8,
          layerId = ~edgeuid,
          popup = ~paste("Edgeuid: ", as.character(edgeuid), "<br>",
                         "Gesamtanzahl Wanderungen: ", as.character(hikers), "<br>",
                         "Gesamtanzahl Radfahrten: ", as.character(bikers), "<br>", 
                         "Segment Laenge: ", as.character(round(m)), " m<br>",
                         "Höchster Punkt: ", as.character(round(Z_Max)), " m ü.M.<br>",
                         "Niedrigster Punkt: ", as.character(round(Z_Min)), " m ü.M.<br>",
                         "Höhendifferenz: ", as.character(round(h_diff)), " m <br>",
                         "Konflikt Index: ", as.character(round(conflict)) 
          ),
          highlightOptions = highlightOptions(color = "yellow", weight = 6)
        ) %>% 
        addLayersControl(baseGroups = c("OSM standard", "Carto dark", "Carto light"),
                         overlayGroups = c("hikers", "bikers", "cluster"),
                         options = layersControlOptions(collapsed = FALSE,
                                                        defaultBase = "Carto dark")) %>% 
        addLegend(
          position = "bottomright",
          pal = color_bike, 
          values = joined_bikers$bikers,
          title = "Bikers Legend",
          opacity = 1
        ) %>%
        addLegend(
          position = "bottomright",
          pal = color_hike, 
          values = joined_hikers$hikers,
          title = "Hikers Legend",
          opacity = 1
        ) %>% 
        showGroup("bikers") %>% 
        showGroup("hikers")

    
    
  })

  

  
# Download function ----------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".zip", sep = "")
    },
    
    content = function(file) {
      # Assuming the filtered shapefile is stored in a reactive called filtered_data()
      filtered_sf <- filtered_bikers()
      filtered_sf$X <- NULL 
      filtered_sf$Z_Min <- NULL 
      
      # Write the shapefile to a temporary directory
      temp_dir <- tempdir()
      sf::st_write(filtered_sf, paste0(temp_dir, "/filtered_data.shp"))
      
      # Zip the shapefile components
      zip(zipfile = file, files = list.files(temp_dir, full.names = TRUE))
    },
    
    contentType = "application/zip"
  )

  
}


shinyApp(ui, server, enableBookmarking = "server")
# Run the Shiny app

