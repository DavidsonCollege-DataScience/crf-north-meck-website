# Load libraries
library(shiny)
library(tidyverse)

# Load and process data (and photos)
ecceData <- read.csv("Childcare Centers - Sheet1.csv") %>%
  mutate(
    Website = case_when(
      Website == "Currently unavailable" ~ "Currently unavailable",
      Website == "Data not available" ~ "Data not available",
      TRUE ~ paste0("<a href='", Website, "' target='_blank'>", Website, "</a>")
    ),
    State_Link = paste0("<a href='", State_Link, "' target='_blank'>", State_Link, "</a>"),
    Photo_File = str_to_lower(Provider_Name),
    Photo_File = str_replace_all(Photo_File, " ", "_"),
    Photo_File = str_replace_all(Photo_File, "[/.-]", "_"),
    Photo_File = str_c(Photo_File, ".jpg"),
    Photo_File = case_when(
      Provider_Name == "The Children’s House" ~ "the_children_s_house.jpg",
      TRUE ~ Photo_File
    ),
    Photo_File = paste0("<img src='", Photo_File, "'>")
  ) %>%
  select (
    Provider_Name, Facility_Type, Address, Ages, Max_Capacity,
    Subsidized_Childcare, Tuition_Insight, License_Number,
    Type, Phone, Website, State_Link, Photo_File
  )

# Define UI
ui <- fluidPage(
  titlePanel("Childcare Information"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("acceptsVouchers", "Offers subsidized childcare", value = FALSE),
      selectInput("center", "Select a licensed childcare center:", choices = NULL)
    ),
    mainPanel(
      htmlOutput("centerInfo")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive filter based on checkbox
  filteredData <- reactive({
    if (input$acceptsVouchers) {
      ecceData %>% filter(Subsidized_Childcare == "Yes")
    } else {
      ecceData
    }
  })
  
  # Update dropdown when checkbox is toggled
  observe({
    updateSelectInput(session, "center", choices = filteredData()$Provider_Name)
  })
  
  # Render center information
  output$centerInfo <- renderUI({
    selected_row <- filteredData()[filteredData()$Provider_Name == input$center, ]
    
    if (nrow(selected_row) == 0) {
      return(HTML("<em>Center data not available.</em>"))
    }
    
    selected_columns <- c(
      "Facility_Type", "Address", "Ages", "Max_Capacity",
      "Subsidized_Childcare", "Tuition_Insight", "License_Number",
      "Type", "Phone", "Website", "State_Link"
    )
    
    labels <- c(
      "Facility Type", "Address", "Ages Served", "Maximum Capacity",
      "Accepts Vouchers", "Tuition Insight", "License Number",
      "License Type", "Phone Number", "Website", "State Provider Lookup"
    )
    
    info_values <- as.character(selected_row[1, selected_columns, drop = FALSE])
    info_lines <- paste0("<strong>", labels, ":</strong> ", info_values, "<br>")
    
    photo_html <- selected_row$Photo_File[1]
    if (!grepl("height=", photo_html)) {
      photo_html <- sub("<img ", "<img height='200' ", photo_html)
    }
    
    HTML(paste0(
      "<strong>", input$center, "</strong><br><br>",
      photo_html, "<br><br>",
      paste(info_lines, collapse = "")
    ))
  })
}

# Run app
shinyApp(ui = ui, server = server)