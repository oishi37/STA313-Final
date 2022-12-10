```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
library(tidyverse)
```

# CODE FOR GGPLOT
```{r}
# this is for incidents by year
temp_data %>% mutate(Year <- as.numeric(Year)) %>% 
  group_by(Year) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = Year, y = count,  group=1)) + 
  geom_line(stat = "identity")

# fatalities per year
temp_data %>% mutate(Year <- as.numeric(Year),
                     Fatalities <- as.numeric(Fatalities)) %>% 
  group_by(Year, Fatalities) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = Year, y = Fatalities,  group=1)) + 
  geom_line(stat = "identity")


# injured per year 
temp_data %>% mutate(Year <- as.numeric(Year),
                     Injured <- as.numeric(Injured)) %>% 
  group_by(Year, Injured) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = Year, y = Injured,  group=1)) + 
  geom_line(stat = "identity")

#victims per year
temp_data %>% mutate(Year <- as.numeric(Year),
                     Total.victims <- as.numeric(Total.victims)) %>% 
  group_by(Year, Total.victims) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = Year, y = Total.victims,  group=1)) + 
  geom_line(stat = "identity")


temp_data %>% mutate(Year <- as.numeric(Year)) %>% 
  group_by(Year, State) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = Year, y = count,  fill = factor(State))) + 
  geom_bar(stat = "identity", position = "fill")

```

# Define UI for app that draws a histogram ----
```{r eruptions, echo=FALSE}
ui <- fluidPage(
  # App title ----
  titlePanel("Us mass shooting"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Dates:",
                  min = as.Date("1966-01-01","%Y-%m-%d"),
                  max = as.Date("2019-12-31","%Y-%m-%d"),
                  value=as.Date("2016-12-01"),timeFormat="%Y-%m-%d"),
      
      selectInput(inputId = "state", label = "State:", 
                 choices = as.character(unique(data$State)), 
                 selected = "Nevada")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)
```
```{r eruptions, echo=FALSE}

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- temp_data$Year
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    temp_data %>% mutate(Year <- as.numeric(Year)) %>% 
       group_by(Year) %>% 
       summarise(count = n()) %>% 
       ggplot(aes(x = Year, y = count,  group=1)) + 
       geom_line(stat = "identity")
    
  })
  
  output$state_selector = renderUI({})
  
}
```

```{r eruptions, echo=FALSE}

shinyApp(ui, server)
```
