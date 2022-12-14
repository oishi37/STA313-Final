library(tidyverse)
library(knitr)
library(shinydashboard)
library(shinyBS)
source("helpers.R")
data <- read.csv("finaldata_313.csv")
federal_laws <- read.csv("federal_laws.csv")
federal_laws <- federal_laws[-(1:4), ]

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
  # App title ----
  titlePanel("US Mass Shootings"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the years of interest ----
      sliderInput(inputId = "years",
                  label = "Year of Interest:",
                  min = 1960,
                  max = 2019,
                  value= c(1960,2019)),
      
      
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

    ),
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Line Graph, clicks represent points ----
      plotOutput(outputId = "timeline", click = "plot_click")
      )
    ), 
  
  fluidRow(
    column(width = 12,
           verbatimTextOutput("click_info")
    )
  )
      
)



# Define server logic
server <- function(input, output, session) {
  
  
  output$timeline <- renderPlot({
    

    
    #creates the filtered data based on the input, returns as a list
    args <- create_temps(input$state, input$years[2], 
                         input$years[1], input$response)
    
    #plots the timeline with input of the function in helpers.R
    plot_timeline(args[[1]], args[[2]], input$response)
  })
  
  #takes the click info and finds the point in data closest to the click
  output$click_info <- renderPrint({
    
    #creates the filtered data based on the input, returns as a list
    args <- create_temps(input$state, input$years[2],
                         input$years[1], input$response)

    #nearpoints matches the click and outputs the data
    temp_year <- nearPoints(args[[1]], input$plot_click,
                            xvar = "Year", yvar = "count")
    
    #this code grabs the corresponding data and outputs it
    law <- federal_laws %>% filter(Year_Implemented %in% temp_year[1])
    cat(paste0("Name: ", law$Name, " [", law$Year_Implemented, 
               "]\nDescription: ", law$Description))
  })

  
}

shinyApp(ui, server)