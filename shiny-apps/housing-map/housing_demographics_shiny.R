library(shiny)
library(sf)
library(dplyr)
library(leaflet)

# Define file paths for pre-saved data
acs_data_path <- "cleaned_acs.rds"
tenure_data_path <- "meck_tenure.rds"
race_tenure_data_path <- "meck_race_tenure.rds"
npa_data_path <- "npa_summary_sf.rds"

# UI
ui <- fluidPage(
  titlePanel("North Meck Housing & Demographics Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("geography", "Select Geography:",
                  choices = c("Census Block Group" = "bg",
                              "Neighborhood Planning Area (NPA)" = "npa")),
      selectInput("variable", "Select Variable:",
                  choices = c(
                    "Renter Share (%)" = "renter_share",
                    "Owner Share (%)" = "owner_share",
                    "Vacancy Rate (%)" = "vacancy_rate",
                    "Median Income ($)" = "med_income",
                    "% White" = "pct_white",
                    "% Black" = "pct_black",
                    "% Asian" = "pct_asian",
                    "% Hispanic" = "pct_hispanic",
                    "White Home Ownership Rate (%)" = "white_own_rate",
                    "Black Home Ownership Rate (%)" = "black_own_rate",
                    "Asian Home Ownership Rate (%)" = "asian_own_rate",
                    "Hispanic Home Ownership Rate (%)" = "hispanic_own_rate"
                  )),
      helpText("Data from 2023 ACS")
    ),
    mainPanel(
      leafletOutput("map", height = "505px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data_store <- reactiveValues(
    acs_data = NULL,
    tenure_data = NULL,
    race_tenure_data = NULL,
    npa_data = NULL,
    loading_errors = character()
  )
  
  # Load datasets with standardized column names
  observe({
    tryCatch({
      data_store$acs_data <- readRDS(acs_data_path) %>%
        rename(GEOID = GEOID_bg)
    }, error = function(e) {
      data_store$loading_errors <- c(data_store$loading_errors, paste("cleaned_acs:", e$message))
    })
    
    tryCatch({
      data_store$tenure_data <- readRDS(tenure_data_path) %>%
        rename(GEOID = GEOID_bg)
    }, error = function(e) {
      data_store$loading_errors <- c(data_store$loading_errors, paste("tenure_data:", e$message))
    })
    
    tryCatch({
      data_store$race_tenure_data <- readRDS(race_tenure_data_path) %>%
        rename(GEOID = GEOID_bg)
    }, error = function(e) {
      data_store$loading_errors <- c(data_store$loading_errors, paste("race_tenure_data:", e$message))
    })
    
    tryCatch({
      data_store$npa_data <- readRDS(npa_data_path)
    }, error = function(e) {
      data_store$loading_errors <- c(data_store$loading_errors, paste("npa_data:", e$message))
    })
  })
  
  # Debug output
  output$debug_info <- renderPrint({
    if (length(data_store$loading_errors) > 0) {
      cat("Loading errors:\n")
      cat(paste(data_store$loading_errors, collapse = "\n"))
    } else {
      cat("Data loaded successfully:\n")
      cat("ACS block groups:", nrow(data_store$acs_data), "rows\n")
      cat("NPA summary:", nrow(data_store$npa_data), "NPAs\n")
    }
  })
  
  # Reactive dataset selector
  selected_data <- reactive({
    req(input$geography, input$variable)
    
    if (input$geography == "bg") {
      if (input$variable %in% names(data_store$acs_data)) {
        return(data_store$acs_data)
      } else if (input$variable %in% names(data_store$race_tenure_data)) {
        return(data_store$race_tenure_data)
      } else {
        return(data_store$tenure_data)
      }
    } else {
      return(data_store$npa_data)
    }
  })
  
  # Render leaflet map
  output$map <- renderLeaflet({
    data <- selected_data()
    req(data)
    
    # Ensure CRS is WGS84
    data <- st_transform(data, crs = 4326)
    
    # Ensure variable exists
    if (!(input$variable %in% names(data))) {
      showNotification("Selected variable not found in dataset.", type = "error")
      return(NULL)
    }
    
    # Filter to non-NA values
    values <- data[[input$variable]]
    if (all(is.na(values))) {
      showNotification("No valid data for this variable.", type = "error")
      return(NULL)
    }
    
    pal <- colorNumeric(palette = "viridis", domain = values, na.color = "transparent")
    
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(data[[input$variable]]),
        fillOpacity = 0.75,
        color = "#444",
        weight = 1,
        label = ~paste0(
          "<strong>", gsub("_", " ", input$variable), ":</strong> ",
          ifelse(is.na(data[[input$variable]]), "NA", round(data[[input$variable]], 1))
        ),
        labelOptions = labelOptions(direction = "auto"),
        highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = values,
        title = input$variable,
        opacity = 1
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
