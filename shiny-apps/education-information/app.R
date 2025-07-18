# Load libraries
library(shiny)
library(tidyverse)

# Load and process data (and photos)
eduData <- read.csv("joined-north-meck-schools.csv") %>% 
  mutate(
    Photo_File = str_to_lower(School_Name),
    Photo_File = str_replace_all(Photo_File, " ", "_"),
    Photo_File = str_replace_all(Photo_File, "[/.]", "_"),
    Photo_File = str_remove_all(Photo_File, "-"),
    Photo_File = str_c(Photo_File, ".jpg"),
    Photo_File = paste0("<img src='", Photo_File, "'>")
  ) %>% 
  select(
    School_Name, School_ID, School_Type, Charter_School, Private_School,
    Address, City, ZIP, Phone_Number, 
    Lowest_Grade, Highest_Grade, School_Level, Photo_File
  )

# Define UI
ui <- fluidPage(
  titlePanel("School Information"),
  
  sidebarLayout(
    sidebarPanel(
      # Checkboxes to filter by school type and level
      checkboxGroupInput(
        "schoolTypeFilter", 
        "Filter by School Type:",
        choices = c("Charter", "Private", "Public (non-charter)"),
        selected = character(0)
      ),
      
      checkboxGroupInput(
        "levelFilter", 
        "Filter by School Level:",
        choices = c("Elementary", "Middle", "Secondary"),
        selected = character(0)
      ),
      
      selectInput("school", "Select a school:", choices = NULL)
    ),
    
    mainPanel(
      htmlOutput("schoolInfo")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Filter data based on user selections
  filteredData <- reactive({
    data <- eduData
    
    if (length(input$schoolTypeFilter) > 0) {
      data <- data %>% filter(
        (("Charter" %in% input$schoolTypeFilter) & Charter_School == "Yes") |
          (("Private" %in% input$schoolTypeFilter) & Private_School == "Yes") |
          (("Public (non-charter)" %in% input$schoolTypeFilter) & 
             Charter_School == "No" & Private_School == "No")
      )
    }
    
    # Filter by school level if any selected (and include other)
    if (length(input$levelFilter) > 0) {
      data <- data %>% filter(School_Level %in% c(input$levelFilter, "Other"))
    }
    
    data
  })
  
  # Update school dropdown when filters change
  observe({
    updateSelectInput(session, "school", choices = filteredData()$School_Name)
  })
  
  # Render school information
  output$schoolInfo <- renderUI({
    selected_row <- filteredData()[filteredData()$School_Name == input$school, ]
    
    if (nrow(selected_row) == 0) {
      return(HTML("<em>School data not available.</em>"))
    }
    
    # Combine address into one line
    full_address <- paste0(selected_row$Address, ", ", selected_row$City, " ", selected_row$ZIP)
    selected_row$Address <- full_address
    selected_row <- selected_row[, !(names(selected_row) %in% c("City", "ZIP"))]
    
    # Combine grade range into one line
    grade_range <- paste0(selected_row$Lowest_Grade, "-", selected_row$Highest_Grade)
    selected_row$Lowest_Grade <- grade_range
    selected_row <- selected_row[, !(names(selected_row) %in% c("Highest_Grade"))]
    
    selected_columns <- c(
      "School_ID", "School_Type", "Charter_School", "Private_School",
      "Address", "Phone_Number", 
      "Lowest_Grade", "School_Level"
    )
    
    labels <- c(
      "School ID", "School Type", "Charter School", "Private School",
      "Address", "Phone Number", 
      "Grade Range", "School Level"
    )
    
    info_values <- as.character(selected_row[1, selected_columns, drop = FALSE])
    info_lines <- paste0("<strong>", labels, ":</strong> ", info_values, "<br>")
    
    photo_html <- selected_row$Photo_File[1]
    
    HTML(paste0(
      "<h3>", input$school, "</h3><br>",
      photo_html, "<br><br>",
      paste(info_lines, collapse = "")
    ))
  })
}

# Run the app
shinyApp(ui = ui, server = server)