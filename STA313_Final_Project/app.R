library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinyBS)
library(ggimage)
source("helpers.R")
data <- read.csv("finaldata_313.csv")
federal_laws <- read.csv("federal_laws.csv")
federal_laws <- federal_laws[-(1:4), ]

ui <- fluidPage(

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
  
  
  tabsetPanel(
    id = "main_tabset",
    tabPanel("1. Introduction",
             tags$h1("Why Gun Control?"),
             tags$h2("Why should we care?"),
             tags$h3("What is our question? *Does the new policies work?*"),
             tags$h4("Big picture, do policies work or not?[our project’s goal is to allow users to figure this out for themselves?]"
    )),
    
    tabPanel("2. State Policy Exploration",
             
             sidebarLayout(
               
               sidebarPanel(
                 
               ),
               
               mainPanel(
                 
               )
               
               
             )
             
             
    ),
    
    tabPanel("3. Federal Policy Exploration",
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 tabsetPanel(
                   id = "tabset",
                   tabPanel("Shooting Characteristics", 
                            # Input: Input for the shooting type
                            selectInput("type", label = "Select which shooting types to display:", 
                                        choices = c("All Types", "Mass (Single Location)",
                                                    "Spree (Multiple Locations)", 
                                                    "Unknown (Missing Information)"), 
                                        selected = "Mass (Single Location)"),
                            
                            bsTooltip("type", "Shootings can be characterized as mass shootings (typically a single location) or shooting sprees (multiple locations)",
                                      "right", options = list(container = "body")),
                            
                            # Input: Input for the motivation
                            selectInput("motivation", label = "Select which causes to display:",
                                        choices = c("All Motivations", sort(as.character(
                                          unique(data$Cause)))),
                                        selected = "All Motivations"),
                            
                            bsTooltip("motivation", "What was the reported cause of the shooting?",
                                      "right", options = list(container = "body")),
                            
                            # Input: Search by keyword
                            textInput("keyword", label = "Search by keyword:", placeholder='Start typing...'),
                            
                            # Input: History of Mental Health
                            checkboxGroupInput("mhhistory", label="History of mental health issues", choices=(c("Yes"))),
                            
                            #Input: Sex
                            checkboxGroupInput("sex", label="Sex", choices=(c('Male', 'Female')), selected = c('Male', 'Female'))
                            )),
                 
                 
                 # Input: Input for the response type ----
                 selectInput("response", 
                             label = "Choose a response to display",
                             choices = c("Shootings",
                                         "Fatalities", "Injuries"),
                             selected = "Shootings"),
                 
                 bsTooltip("response", "Select whether the response indicates the number of shootings, the number of fatalities or the number of non-fatal injuries for a given year",
                           "right", options = list(container = "body")),
                 
                 
                 # Input: Input for the states
                 selectInput("state", label = "Select which states to display:", 
                             choices = c("All States", sort(as.character(
                               unique(data$State)))), 
                             selected = "All States",
                             multiple = TRUE),
                 
                 bsTooltip("state", "Click which state(s) to examine in the timeline",
                           "right", options = list(container = "body")),
                 
               ),
               
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Line Graph, clicks represent points ----
                 plotOutput(outputId = "timeline", click = "plot_click"),
                 
                 tags$br(),
                 
                 # Input: Slider for the years of interest ----
                 wellPanel(
                 sliderInput(inputId = "years",
                             label = "Year of Interest:",
                             min = 1960,
                             max = 2019,
                             value= c(1960,2019))
               )
               )
             ),
             
             tags$br(),
             fluidRow(
               column(width = 12, verbatimTextOutput("click_info"),
                      align = "center"
               ),
             )
  ),
  
  tabPanel("4. Conclusions/Limitations"),
  
  footer = tags$p("Dataset was taken from ", tags$a("kaggle.com.", 
                                                    href = "https://www.kaggle.com/datasets/myho63/us-mass-shooting-1966-2019")
    ),
  )
)



# Define server logic
server <- function(input, output, session) {
  
  
  #creates the filtered data based on the input, returns as a list
  args <- reactive({
    create_temps(input$state, input$years[2],
                 input$years[1], input$response, input$type,
                 input$motivation, input$keyword, input$mhhistory, input$sex)
  })
  
  output$timeline <- renderPlot({
    
    
    #plots the timeline with input of the function in helpers.R
    plot_timeline(args()[[1]], args()[[2]], input$response)
  })
  
  #takes the click info and finds the point in data closest to the click
  output$click_info <- renderPrint({
  
    #nearpoints matches the click and outputs the data
    temp_year <- nearPoints(args()[[1]], input$plot_click,
                            xvar = "Year", yvar = "count")
    
    #this code grabs the corresponding data and outputs it
    law <- federal_laws %>% filter(Year_Implemented %in% temp_year[1])
    cat(paste0(law$Name, " [", law$Year_Implemented, "]\n", law$Description))
    
  })

  
  
}

shinyApp(ui, server)
