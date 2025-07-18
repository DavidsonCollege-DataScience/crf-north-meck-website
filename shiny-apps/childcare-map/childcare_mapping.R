library(shiny)
library(leaflet)
library(dplyr)
library(r5r)
library(sf)
library(rsconnect)

# Load your data (make sure childcare_points_df file and the nc_pbf folder 
# are in the same folder)
childcare_points_df <- read.csv(file.path(getwd(), "childcare_points_df.csv"))
childcare_sf <- st_as_sf(childcare_points_df, coords = c("lon", "lat"), crs = 4326)

# Set Java heap *before* any rJava loads
options(java.parameters = "-Xmx6G")
library(rJava)
.jinit()
r5r_core <- setup_r5(file.path(getwd(), "nc_pbf"))

# UI
ui <- fluidPage(
  titlePanel("Childcare Access Map"),
  checkboxInput("filter_vouchers", "Show only centers that accept vouchers", value = FALSE),
  leafletOutput("map", height = "600px"),
  verbatimTextOutput("clicked_coords"),
  downloadButton("download_data", "Download Results as CSV")
)

# Server
server <- function(input, output, session) {
  
  clicked <- reactiveVal(NULL)
  results_reactive <- reactiveVal(NULL)
  
  # Reactive dataset that filters based on voucher checkbox
  filtered_childcare_sf <- reactive({
    if (input$filter_vouchers) {
      childcare_sf %>% filter(tolower(Vouchers) == "yes")
    } else {
      childcare_sf
    }
  })
  
  filtered_childcare_points_df <- reactive({
    if (input$filter_vouchers) {
      childcare_points_df %>% filter(tolower(Vouchers) == "yes")
    } else {
      childcare_points_df
    }
  })
  
  # Initial map with childcare centers
  output$map <- renderLeaflet({
    leaflet(filtered_childcare_sf()) %>%
      addTiles() %>%
      addCircleMarkers(
        color = "gray", radius = 4,
        popup = ~id
      )
  })
  
  observeEvent(input$map_click, {
    lat <- input$map_click$lat
    lon <- input$map_click$lng
    clicked(c(lat, lon))
    
    user_origin <- data.frame(id = "user", lat = lat, lon = lon)
    
    travel_times <- travel_time_matrix(
      r5r_core,
      origins = user_origin,
      destinations = filtered_childcare_points_df(),
      mode = "CAR",
      departure_datetime = as.POSIXct("2025-06-28 08:00:00"),
      max_trip_duration = 60,
      time_window = 1,
      percentiles = 50
    ) %>%
      rename(travel_time = travel_time_p50)
    
    if (nrow(travel_times) == 0) {
      showNotification("No reachable centers within time limit.", type = "error")
      return()
    }
    
    result <- left_join(travel_times, filtered_childcare_sf(), by = c("to_id" = "id"))
    
    if (!("lon" %in% names(result)) || !("lat" %in% names(result))) {
      coords <- st_coordinates(result$geometry)
      result$lon <- coords[, 1]
      result$lat <- coords[, 2]
    }
    
    result <- result %>%
      mutate(popup_text = paste0("<b>", to_id, "</b><br>Travel time: ", travel_time, " min"))
    
    results_reactive(result)
    
    nearest <- result %>% filter(travel_time == min(travel_time, na.rm = TRUE))
    
    leafletProxy("map", data = result) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addTiles() %>%
      addMarkers(lng = lon, lat = lat, popup = "Your location", label = "You") %>%
      addCircleMarkers(
        data = result %>% filter(to_id != nearest$to_id[1]),
        lng = ~lon, lat = ~lat,
        color = "blue", radius = 4, fillOpacity = 0.8,
        popup = ~popup_text
      ) %>%
      addCircleMarkers(
        data = nearest,
        lng = ~lon, lat = ~lat,
        color = "red", radius = 6, fillOpacity = 1,
        popup = ~paste0("<b>Nearest: ", to_id, "</b><br>Time: ", travel_time, " min")
      )
  })
  
  output$clicked_coords <- renderPrint({
    if (!is.null(clicked())) {
      paste("Clicked location:", clicked()[1], ",", clicked()[2])
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("childcare_access_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(results_reactive(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)
