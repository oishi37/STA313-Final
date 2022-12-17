library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinyBS)
library(ggimage)
source("helpers.R")
data <- read.csv("finaldata_313.csv")
federal_laws <- read.csv("federal_laws.csv")
federal_laws <- federal_laws[-(1:4), ]
policies <- read.csv("policies.csv")

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
    tabPanel("1. INTRODUCTION",
             
             tags$br(),
             
             wellPanel(
             tags$h1("Why Should We Care About Gun Control?"),
             strong("This evidence-based graph from Small Arms Survey (Figure 1.0), shows us that for every 100 US residents, there are 120.5 guns in possession. This ratio significantly surpasses any other country in the world. \n Furthermore, in the last decade, there has been a spike in violent acts regarding mass shootings, thus, it is valuable to address the concerns about gun control and increase understanding about it.")),
             
             fluidRow(
               
               
               column(width = 6,
                      wellPanel(tags$h1("Does Gun Control Save Lives? "),
                      strong("In light of these violent acts, US governments have attempted to proactively take action to propose new policies to mitigate the number of mass shootings. Over the decades there have been federal acts and bans put in place, as well as policies implemented by state. However, has the introduction of these legislations been effective?"),
                      
                      tags$br(),
                      tags$br(),
                      
                      strong("Our data-driven article aims to provide users with all the information and data they need to explore how different US policies affect uniquely categorized mass shootings. It is intended that users analyze and understand if these government legislations are doing an effective job of mitigating mass shooting incidents in the US."))),
               
               column(width = 6, 
                    tags$figure(align = "center",
                                tags$img(
                                  src = "https://thequakercampus.org/wp-content/uploads/2021/04/small-arms-survey.png",
                                  width = 500,
                                  align = "center")),
                    tags$p("Figure 1.0: Adapted from", tags$a("BBC.com.", 
                                                                      href = "https://www.bbc.com/news/world-us-canada-41488081"))
                                    
             
             
             )
             )),
    
    tabPanel("2. STATE POLICY EXPLORATION",
             
             sidebarLayout(
               
               sidebarPanel(
                 #state years
                 sliderInput(inputId = "stateyears",
                             label = "Year of Interest:",
                             min = 1960,
                             max = 2019,
                             value= c(1980,2019)),
                 
                 tabsetPanel(
                   tabPanel("Background Checks",
                            checkboxGroupInput("bc",
                                               label = "Level of background checks:",
                                               choiceNames = c("Dealers Only",
                                                               "Private Sale", "Permit to Purchase"),
                                               choiceValues = c("X2", 
                                                                "X3", "X4"),
                                               selected = c("X2")
                                               )),
                   
                   bsTooltip("bc", "Background checks can be required for state-wide, private sales or when purchasing.",
                             "right", options = list(container = "body")),
                   
                   tabPanel("Concealed Carry",
                            checkboxGroupInput("cc",
                                               label = "Concealed carry permit requirements:",
                                               choiceNames = c("Not Required", 
                                                               "Shall Issue", 
                                                               "May Issue", 
                                                               "Prohibited"),
                                               choiceValues = c("X1", "X2", 
                                                                "X3", "X4"),
                                               )),
                   
                   bsTooltip("cc", "Weapon carrying can be completely unregulated, for non-prohibited personnel, up to the discretion of the state or only for officers.",
                             "right", options = list(container = "body")),
                   
                   tabPanel("Self Defense",
                            checkboxGroupInput("sd",
                                               label = "Self defense legislation:",
                                               choiceNames = c("Castle Doctrine", 
                                                               "Expanded 1", 
                                                               "Expanded 2", 
                                                               "Stand your ground"),
                                               choiceValues = c("X1", "X2", 
                                                                "X3", "X4"),
                                               )),
                   
                   
                   bsTooltip("sd", "Individuals can act in self defence if they are in their home, in their home/work/car, beyond their work/home/car or regardless of where they are.",
                             "right", options = list(container = "body")),
                   
                   tabPanel("Child Access",
                            checkboxGroupInput("ca",
                                               label = "Child access legislation",
                                               choiceNames = c("Reckless Provision", 
                                                               "Negligent Storage"),
                                               choiceValues = c("X1", "X2")),
                   
                   
                   bsTooltip("ca", "States can either charge adults for providing handguns to children, or for leaving their handguns in an accessible environment.",
                             "right", options = list(container = "body")))
                 
                 
               )
               
               ),
               
               mainPanel(
                 
                  plotOutput(outputId = "state_timeline"),
                  
                 
               )),
               
               wellPanel(verbatimTextOutput("state_lists"),
                        align = "center"
                 )
               
               
             
             
             
    ),
    
    tabPanel("3. FEDERAL POLICY EXPLORATION",
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Input: Input for the response type ----
                 selectInput("response", 
                             label = "Choose a response to display",
                             choices = c("Shootings",
                                         "Fatalities", "Injuries"),
                             selected = "Shootings"),
                 
                 bsTooltip("response", "Select whether the response indicates the number of shootings, the number of fatalities or the number of non-fatal injuries for a given year",
                           "right", options = list(container = "body")),
                 
                 tabsetPanel(
                   id = "tabset",
                   tabPanel("Shooting Filters", 
                            # Input: Input for the shooting type
                            selectInput("type", label = "Select which shooting types to display:", 
                                        choices = c("All Types", "Mass (Single Location)",
                                                    "Spree (Multiple Locations)", 
                                                    "Unknown (Missing Information)"), 
                                        selected = "All Types"),
                            
                            bsTooltip("type", "Shootings can be characterized as mass shootings (typically a single location) or shooting sprees (multiple locations)",
                                      "right", options = list(container = "body")),
                            
                            # Input: Search by keyword
                            textInput("keyword", label = "Search by keyword:", placeholder='Start typing...'),
                            
                            bsTooltip("keyword", "Filter for keywords in the description of the event as well as the perpetrator's mental health profile",
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
                   
                   tabPanel("Perpetrator Filters",
                            # Input: Input for the motivation
                            selectInput("motivation", label = "Select which causes to display:",
                                        choices = c("All Motivations", sort(as.character(
                                          unique(data$Cause)))),
                                        selected = "All Motivations"),
                            
                            bsTooltip("motivation", "What was the reported cause of the shooting?",
                                      "right", options = list(container = "body")),
                            
                            #Input: Weapon
                            checkboxGroupInput("weapon", label="Weapons Used", 
                                               choiceNames =c('Rifle','Handgun', 
                                                              'Shotgun',
                                                              'Unknown (Missing Data)'),
                                               choiceValues = c("rifle", "handgun|pistol|revolver", "shotgun", "unknown"),
                                               selected = c("rifle", "handgun|pistol|revolver", "shotgun", "unknown")),
                            
                            bsTooltip("weapon", "Type of weapon(s) used by the shooter. Note that some shooters used multiple kinds of weapons.",
                                      "right", options = list(container = "body")),
                            
                            # Input: History of Mental Health
                            checkboxGroupInput("mhhistory", label="History of mental health issues", choices=(c("Yes"))),
                            
                            bsTooltip("mhhistory", "Did the shooter have previous confirmed mental health issues?",
                                      "right", options = list(container = "body")),
                            
                            #Input: Sex
                            checkboxGroupInput("sex", label="Sex", choices=(c('Male', 'Female')), selected = c('Male', 'Female')),
                            
                            bsTooltip("sex", "Sex of the perpetrator, or one of the perpetrators if more than one",
                                      "right", options = list(container = "body")))
                   ),
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
  
  tabPanel("4. CONCLUSION",
           
           wellPanel(
             tags$h3("Concluding Remarks"),
             strong("We see that US gun legislations have a long way to go before they are deemed effective. Many legislations were seen to produce drastic spikes in mass shootings while others provided relief for a limited amount of time.")
             ),
             
             wellPanel(
               tags$h3("Limitations"),
               strong("The visualization does not weigh the number of deaths/victims by the population of the state. When the visualization is accounted for by all the states on a federal level, it is not an issue. However, when we see the data in terms of the states, the number of deaths/victims does not account for the population of the state. It might be the case that a state has a large number of deaths because it has a larger population.")
           
           )),
  
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
                 input$motivation, input$keyword, input$mhhistory, 
                 input$sex, input$weapon)
  })
  
  #creates the filtered data based on the state input
  state_args <- reactive({
    create_statelist(input$stateyears[1], input$stateyears[2],
                     input$bc, input$cc, input$sd, input$ca)
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
  
  
  output$state_timeline <- renderPlot({
    
    plot_states(input$stateyears[1], input$stateyears[2], state_args())
  })

  output$state_lists <- renderPrint({
    s <- get_states(input$stateyears[1], input$stateyears[2], state_args())
    
    cat("Implemented States \n", s[[1]], "\n\nNot Implemented States\n", s[[2]])
  })
  
}

shinyApp(ui, server)
