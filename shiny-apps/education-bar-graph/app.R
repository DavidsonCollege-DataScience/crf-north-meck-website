# Load libraries
library(shiny)
library(tidyverse)
library(viridis)
library(scales)
library(plotly)

# Load and process data
joined_schools <- read_csv("Joined_NMeck_Schools_Data.csv") %>% 
  mutate(School_Name = case_when(
    School_Name == "William Amos Hough High" ~ "William Amos Hough High",
    TRUE ~ School_Name
  ))

demog_pcts <- joined_schools %>% 
  select(School_ID, School_Name, starts_with("pct")) %>% 
  pivot_longer(
    cols = starts_with("pct"),
    names_to = "Race",
    values_to = "Pct"
  ) %>% 
  mutate(
    Race = case_when(
      Race == "pct_AIAN_Students"   ~ "American Indian/Alaska Native",
      Race == "pct_AAPI_Students"   ~ "Asian American/Pacific Islander",
      Race == "pct_Hispanic_Students" ~ "Hispanic",
      Race == "pct_Black_Students"  ~ "Black or African American",
      Race == "pct_White_Students"  ~ "White",
      Race == "pct_NHPI_Students"   ~ "Native Hawaiian or Other Pacific Islander",
      Race == "pct_Two_More_Students" ~ "Two or more races",
      TRUE ~ Race
    ),
    Nickname = str_sub(School_Name, 1, 15)
  ) %>%
  filter(!is.na(School_Name)) # clean up just in case

valid_schools <- demog_pcts %>%
  group_by(School_Name) %>%
  filter(!any(is.na(Pct))) %>%
  ungroup()

school_list <- sort(unique(valid_schools$School_Name))

# Custom theme and color palette

my_palette <- c(
  "#E69F00",  # orange
  "#56B4E9",  # sky blue
  "#009E73",  # bluish green
  "#F0E442",  # yellow
  "#0072B2",  # blue
  "#D55E00",  # vermillion
  "#CC79A7"   # reddish purple
)

my_scale <- scale_fill_manual(
  values = my_palette,
  name = "Student Race/Ethnicity",
  guide = guide_legend(
    position = "bottom",
    keyheight = unit(3, units = "mm"),
    keywidth = unit(50, units = "mm"),
    label.position = "bottom", 
    title.position = "top", 
    nrow = 3
  )
)

my_theme <- theme(
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
  axis.text.y = element_text(size = 9, hjust = 1, color = "black"),
  axis.text.x = element_text(size = 9, color = "black"),
  legend.text = element_text(size = 9),
  legend.title = element_text(size = 10),
  legend.justification = c(0, 0),
  legend.margin = margin(5, 5, 5, 5, unit = "pt"),
  legend.box.margin = margin(5, 5, 5, 5, unit = "pt")
)

# Define UI

ui <- fluidPage(
  titlePanel("Student Enrollment Breakdown by Race/Ethnicity"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select schools to compare:"),
      
      fluidRow(
        column(6, actionButton("select_all", "Select All")),
        column(6, actionButton("deselect_all", "Deselect All"))
      ),
      
      checkboxGroupInput(
        inputId = "schools",
        label = NULL,
        choices = school_list,
        selected = school_list
      )
    ),
    
    mainPanel(
      plotlyOutput("racePlot", height = "800px")
    )
  )
)

# Define server

server <- function(input, output, session) {
  
  school_list <- sort(unique(demog_pcts$School_Name))  # <- move this here
  
  filtered_data <- reactive({
    req(input$schools)
    demog_pcts %>%
      filter(School_Name %in% input$schools) %>%
      filter(!is.na(Pct))
  })
  
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(
      session,
      inputId = "schools",
      selected = school_list
    )
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(
      session,
      inputId = "schools",
      selected = character(0)
    )
  })
  
  output$racePlot <- renderPlotly({
    p <- filtered_data() %>%
      ggplot() +
      geom_col(
        aes(
          x = fct_rev(factor(School_Name)),
          y = Pct,
          fill = Race,
          text = paste0(
            "<b>", School_Name, "</b><br>",
            "Race: ", Race, "<br>",
            "Percentage: ", percent(Pct, accuracy = 0.1)
          )
        ),
        position = "stack",
        width = 0.8
      ) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.05))) +
      theme_void() +
      my_scale +
      my_theme
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.25
        ),
        margin = list(l = 100, r = 20, t = 20, b = 100),
        hoverlabel = list(
          font = list(size = 11),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)  # Hides plotly toolbar
  })
}
  

# Run the app
shinyApp(ui = ui, server = server)