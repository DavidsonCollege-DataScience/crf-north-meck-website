library(shiny)
library(tidyverse)
library(viridis)
library(scales)

# ----- Load and preprocess the data -----

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

# ----- Custom theme and palette -----

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
  legend.justification = c(0, 0),
  legend.margin = margin(5, 5, 5, 5, unit = "pt"),
  legend.box.margin = margin(5, 5, 5, 5, unit = "pt"), 
  axis.text.y = element_text(size = 7, hjust = 1),
  axis.text.x = element_text(size = 7)
)

# ----- UI -----

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
      plotOutput("racePlot", height = "800px")
    )
  )
)

# ----- Server -----

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
  
  output$racePlot <- renderPlot({
    filtered_data() %>%
      ggplot() +
      geom_col(
        aes(
          x = fct_rev(factor(School_Name)),
          y = Pct,
          fill = Race
        )
      ) +
      coord_flip() +
      theme_void() +
      my_scale +
      my_theme +
      scale_y_continuous(labels = label_percent(scale = 1))
  })
}
  

# ----- Run the app -----
shinyApp(ui = ui, server = server)