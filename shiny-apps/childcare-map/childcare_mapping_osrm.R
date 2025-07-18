library(shiny)
library(leaflet)
library(dplyr)
library(osrm)
library(sf)

# Load childcare data
childcare_points_df <- read.csv("childcare_points_df.csv")
childcare_sf <- st_as_sf(childcare_points_df, coords = c("lon", "lat"), crs = 4326)

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
  
  # Filter based on voucher checkbox
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
  
  # Initial map
  output$map <- renderLeaflet({
    leaflet(filtered_childcare_sf()) %>%
      addTiles() %>%
      addCircleMarkers(
        color = "gray", radius = 4,
        popup = ~id
      )
  })
  
  # Handle map click
  observeEvent(input$map_click, {
    lat <- input$map_click$lat
    lon <- input$map_click$lng
    clicked(c(lat, lon))
    
    # Convert origin and destination to sf
    origin <- st_as_sf(data.frame(id = "user", lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
    
    dest_coords <- st_coordinates(filtered_childcare_sf())
    dest_df <- data.frame(
      id = filtered_childcare_sf()$id,
      lon = dest_coords[, "X"],
      lat = dest_coords[, "Y"],
      stringsAsFactors = FALSE
    )
    dest_sf <- st_as_sf(dest_df, coords = c("lon", "lat"), crs = 4326)
    
    # Compute travel times
    travel_result <- tryCatch({
      osrmTable(src = origin, dst = dest_sf)
    }, error = function(e) {
      showNotification(paste("Routing failed:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(travel_result) || is.null(travel_result$durations)) {
      showNotification("No travel time data returned.", type = "error")
      return()
    }
    
    travel_times <- as.vector(travel_result$durations)
    if (all(is.na(travel_times))) {
      showNotification("No reachable centers from selected location.", type = "warning")
      return()
    }
    
    # Merge back with childcare points
    merged <- filtered_childcare_sf() %>%
      mutate(
        travel_time = travel_times,
        popup_text = paste0("<b>", id, "</b><br>Travel time: ", round(travel_time, 1), " min")
      ) %>%
      filter(!is.na(travel_time))
    
    results_reactive(merged)
    
    # Find nearest
    nearest <- merged %>% filter(travel_time == min(travel_time, na.rm = TRUE))
    
    # Update map
    leafletProxy("map", data = merged) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addTiles() %>%
      addMarkers(lng = lon, lat = lat, popup = "Your location", label = "You") %>%
      addCircleMarkers(
        data = merged %>% filter(id != nearest$id[1]),
        color = "blue", radius = 4, fillOpacity = 0.8,
        popup = ~popup_text
      ) %>%
      addCircleMarkers(
        data = nearest,
        color = "red", radius = 6, fillOpacity = 1,
        popup = ~paste0("<b>Nearest: ", nearest$id[1], "</b><br>Time: ", round(nearest$travel_time, 1), " min")
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

shinyApp(ui = ui, server = server)
