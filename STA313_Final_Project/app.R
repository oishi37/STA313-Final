library(tidyverse)
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
      
      selectInput("response1", 
                  label = "Choose a response to display",
                  choices = c("Mass", "Spree"),
                  selected = "Shooting Types"),
      
      selectInput("response2", 
                  label = "Choose a response to display",
                  choices = c("Racial",
                              "Religious", "Unknown"),
                  selected = "Motivation")
           
      
      
    ),
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Line Graph ----
      plotOutput(outputId = "timeline")
      
    )
  )
)



# Define server logic
server <- function(input, output) {
  
  output$timeline <- renderPlot({
    
    
    #takes input from responseID, switch function assigns values to the
    #different inputs
    response <- switch(input$response, 
                       "Shootings" = "Shootings",
                       "Fatalities" = "Fatalities",
                       "Injuries" = "Injured")
    response1 <- switch(input$response1, 
                       "Mass" = "Mass",
                       "Spree" = "Spree")
    response2 <- switch(input$response2, 
                       "Racial" = "",
                       ")

    
    #plots the timeline with input of the function in helpers.R
    plot_timeline(response, input$years[1], input$years[2])
  })
  
}

shinyApp(ui, server)
