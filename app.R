
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
#trails <- st_read("data/Wegenetz/Wegenetz_small.shp") 
trails <- st_read("data/Wegenetz/Wegenetz_raw_activity_studyarea.shp") 
#trails_small <- trails[1:5000, ]
#
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
                              div(actionButton("reset", "Reset drawing filter"), style = "margin-bottom: 10px;"),
                              div(downloadButton("downloadBikingData", "Download Filtered Biking Shapefile"), style = "margin-bottom: 10px;"),
                              div(downloadButton("downloadHikingData", "Download Filtered Hiking Shapefile"))
                              
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
                          HTML("
       <div style='margin: 10px; padding: 10px;'>
       
         <h3>How to use this Dashboard</h3>
         <p>
           The dashboard displays activity data of hiking and biking, recorded by Strava users over the years 2019-2022. The display of hiking and biking layers can be toggled on the top right corner. The temporal distribution of the data displayed is shown at the beginning in the plots at the top. When clicking on a segment, further information of the segment is displayed in a popup, regarding total activity numbers and segment attributes such as height, length, etc., and the plots at the top show the temporal distribution of all activities recorded on that segment. Additionally, the dashboard allows for multiple filtering of the data.
         </p>
         <ul>
           <li>The top kilometer filter filters the data according to the trails with the most activity e.g., select the top 200 km with the most rides.</li>
           <li> The altitude filter uses the highest point of each segment for filtering </li>
           <li>The height difference filters for the height meters overcome in the segments.</li>
           <li>A special feature is the toggle filter. When applied, the current map extent is used to select the map bounds for the subsequent filters. To restore the full trail network, simply untoggle the switch.</li>
         </ul>
         
         
         <h3>Conflict Index</h3>
         <p>
           The conflict index is derived by normalizing the total trips values from both the biking and hiking datasets. This index ranges from 0 (no conflict) to 100 (maximum conflict), with the highest conflict occurring when both bikers and hikers record a high number of total trips for a specific edgeUID. It's important to note that both the biking and hiking layers display the same conflict index for the corresponding edgeuid.
         </p>

         <h3>Code</h3>
         <p>Discover the code and input data that were utilized to develop this Shiny mapping tool on 
           <a href='https://github.com/clairepg/salzbike' target='_blank'>Github</a>.
         </p>
         
         <h3>Data Sources</h3>
         
         <div style='display: flex; align-items: start; margin-bottom: 20px;'>
           <img src='strava_logo.png' style='margin-right: 20px; width: 100px; height: 100px;'>
           <div>
             <h4>Strava Metro</h4>
             <p>The activity data is from Strava Metro, a service by Strava where athletes can upload their running, cycling, etc., activities. The data utilized by this application includes biking data from 2019 to 2022 and walking data (which encompasses running, hiking, walking) from 2019 to 2021.</p>
           </div>
         </div>
         
         <div style='display: flex; align-items: start; margin-bottom: 20px;'>
           <img src='Openstreetmap_logo.svg.png' style='margin-right: 20px; width: 100px; height: 100px;'>
           <div>
             <h4>OpenStreetMap API</h4>
             <p>Data from forest roads, trails, and dirt tracks was extracted using the OpenStreetMap API to exclude data from asphalt roads from the analysis. Note that the road surface data from OpenStreetMap is not 100% accurate, so data from regular streets might also be included in the displayed data.</p>
           </div>
         </div>
         
         <h4>Contact</h4>

       </div>
       ")
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
        mutate(cumulative_km = cumsum(km)) %>% 
        filter(cumulative_km <= km_filter[2])
      data_hikers <- data_hikers %>% 
        arrange(desc(hikers)) %>% 
        mutate(cumulative_km = cumsum(km)) %>% 
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
  filtered_hikers <- reactiveVal()
  
# observe block applying filters

  observe({

    filtered_bikers_data <- combined_filtered_data()$bikers
    filtered_hikers_data <- combined_filtered_data()$hikers
    print(str(filtered_hikers_data))
    
    #extra line for download data
    filtered_bikers(combined_filtered_data()$bikers)
    filtered_hikers(combined_filtered_data()$hikers)
    
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
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
  output$downloadBikingData <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".zip", sep = "")
    },
    
    content = function(file) {
      # Assuming the filtered shapefile is stored in a reactive called filtered_data()
      filtered_sf <- filtered_bikers()
      filtered_sf$X <- NULL 
      filtered_sf$Z_Min <- NULL 
      
      # Create a unique path for the shapefile
      unique_path <- tempfile("filtered_biking_data")
      unique_shp <- paste0(unique_path, ".shp")
      
      # Write the shapefile to the unique path
      sf::st_write(filtered_sf, unique_shp)
      
      # Define the directory of the shapefile
      temp_dir <- dirname(unique_path)
      
      # Use the system's zip command
      files <- list.files(temp_dir, pattern = "filtered_biking_data*", full.names = TRUE)
      file_names <- basename(files)
      cmd <- sprintf("zip %s -j %s", shQuote(file), paste(shQuote(files), collapse = " "))
      system(cmd)
    },
    
    contentType = "application/zip"
  )
  
  
  output$downloadHikingData <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".zip", sep = "")
    },
    
    content = function(file) {
      # Assuming the filtered shapefile is stored in a reactive called filtered_data()
      filtered_sf <- filtered_hikers()
      filtered_sf$X <- NULL 
      filtered_sf$Z_Min <- NULL 
      
      # Create a unique path for the shapefile
      unique_path <- tempfile("filtered_hiking_data")
      unique_shp <- paste0(unique_path, ".shp")
      
      # Write the shapefile to the unique path
      sf::st_write(filtered_sf, unique_shp)
      
      # Define the directory of the shapefile
      temp_dir <- dirname(unique_path)
      
      # Use the system's zip command
      files <- list.files(temp_dir, pattern = "filtered_hiking_data*", full.names = TRUE)
      file_names <- basename(files)
      cmd <- sprintf("zip %s -j %s", shQuote(file), paste(shQuote(files), collapse = " "))
      system(cmd)
    },
    
    contentType = "application/zip"
  )
  
  
  
  
}


shinyApp(ui, server, enableBookmarking = "server")
# Run the Shiny app

