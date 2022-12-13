library(tidyverse)
library(shinydashboard)
library(shinyBS)
source("helpers.R")
data <- read.csv("finaldata_313.csv")

ui <- fluidPage(
  

  
  # App title ----
  titlePanel("US Mass Shootings"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the years of interest ----
      sliderInput(inputId = "years",
                  label = "Year of Interest:",
                  min = 1966,
                  max = 2019,
                  value= c(1966,2019)),
      
      
      # Input: Input for the response type ----
      selectInput("response", 
                  label = "Choose a response to display",
                  choices = c("Shootings",
                              "Fatalities", "Injuries"),
                  selected = "Shootings"),
      
      # Input: Input for the states
      selectInput("state", label = "Select which states to display:", 
                  choices = c("All States", sort(as.character(
                    unique(data$State)))), 
                  selected = "All States",
                  multiple = TRUE),
      
      bsPopover("policies", "Policy Name", 
                placement = "bottom", 
                trigger = "hover")
    ),
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Line Graph ----
      plotOutput(outputId = "timeline")
      
    )
  )
)



# Define server logic
server <- function(input, output, session) {
  
  output$timeline <- renderPlot({
    
    
    #takes input from responseID, switch function assigns values to the
    #different inputs
    response <- switch(input$response, 
                       "Shootings" = "Shootings",
                       "Fatalities" = "Fatalities",
                       "Injuries" = "Injured")
    
    #takes input from state
    if("All States" %in% input$state){
      states <- as.character(unique(data$State))
    } else {
      states <- input$state
    }

    
    #plots the timeline with input of the function in helpers.R
    plot_timeline(response, input$years[1], input$years[2], states)
  })
  
  addPopover(session, "timeline", "Data", content = paste0("hello")
             , trigger = 'hover')
  
}

shinyApp(ui, server)