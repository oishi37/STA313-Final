library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggimage)
source("helpers.R")
data <- read.csv("finaldata_313.csv")
federal_laws <- read.csv("federal_laws.csv")
federal_laws <- federal_laws[-(1:4), ]

ui <- fluidPage(
  # 
  # tags$h1("First level heading"), 
  # tags$h2("Second level heading"), 
  # tags$h3("Third level heading"),
  # 
  # theme = bslib::bs_theme(bootswatch = "darkly"),

  #background
  fluidRow(
    #tags$head(tags$style(type="text/css",
     #                           "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")),
     #      tags$h2(""),
           setBackgroundImage(
      src = "https://st2.depositphotos.com/1031914/10245/i/950/depositphotos_102458582-stock-photo-faded-vintage-paper-with-red.jpg")),


  # App title ----
  titlePanel("Gun Control: Does it Actually Work?"),
  
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
    
    # Input: Input for the shooting type
    selectInput("type", label = "Select which shooting types to display:", 
                choices = c("All Types", "Mass (Single Location)",
                            "Spree (Multiple Locations)", 
                            "Unknown (Missing Information)"), 
                selected = "All Types"),
    
    # Input: Input for the motivation
    selectInput("motivation", label = "Select which causes to display:",
                choices = c("All Motivations", sort(as.character(
                  unique(data$Cause)))),
                selected = "All Motivations"),
  ),
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Line Graph, clicks represent points ----
      plotOutput(outputId = "timeline", click = "plot_click")
      )
    ), 
  
  tags$br(),
  tags$p("Dataset was taken from ", tags$a("kaggle.com.", 
         href = "https://www.kaggle.com/datasets/myho63/us-mass-shooting-1966-2019")
         ),
  fluidRow(
    column(width = 12, verbatimTextOutput("click_info"),
           align = "center"
    )
  )
  
)



# Define server logic
server <- function(input, output, session) {
  
  
  output$timeline <- renderPlot({
    
    
    #creates the filtered data based on the input, returns as a list
    args <- create_temps(input$state, input$years[2], 
                         input$years[1], input$response, input$type, 
                         input$motivation)
    
    #plots the timeline with input of the function in helpers.R
    plot_timeline(args[[1]], args[[2]], input$response)
  })
  
  #takes the click info and finds the point in data closest to the click
  output$click_info <- renderPrint({
    
    #creates the filtered data based on the input, returns as a list
    args <- create_temps(input$state, input$years[2],
                         input$years[1], input$response, input$type,
                         input$motivation)

    #nearpoints matches the click and outputs the data
    temp_year <- nearPoints(args[[1]], input$plot_click,
                            xvar = "Year", yvar = "count")
    
    #this code grabs the corresponding data and outputs it
    law <- federal_laws %>% filter(Year_Implemented %in% temp_year[1])
    cat(paste0(law$Name, " [", law$Year_Implemented, "]\n", law$Description))
    
  })

  
  
}

shinyApp(ui, server)
