data = read.csv("finaldata_313.csv")

#when adding filters, make sure to add filter parameters here
plot_timeline <- function(response, min = 1966, max = 2019) {
  
  #add any additional filters here
  temp <- data %>% filter(Year < max) %>% filter(Year > min)
  
  #tried to make this cleaner by passing arguments into ggplot code
  #didnt work, problems with evaluating column names so just hard coded
  if(response == "Shootings"){
  product <- temp %>% group_by(Year) %>% summarise(count = n()) %>% 
    ggplot(aes(x = Year, y = count)) + geom_line() +
    labs(title= paste0("Timeline of ", response, " in the US"),
         x = "Year", 
         y = paste0("Number of ", response)) + theme_classic()
  
  } else if(response == "Fatalities") {
    product <- temp %>% group_by(Year, Fatalities) %>% 
      summarise(count = n()) %>% ggplot(aes(x = Year, y = Fatalities)) + 
                                          geom_line() +
      labs(title = "Timeline of Fatalities in the US",
           x = "Year", 
           y = "Number of Fatalities") + theme_classic()
  } else {
    product <- temp %>% group_by(Year, Injured) %>% 
      summarise(count = n()) %>% ggplot(aes(x = Year, y = Injured)) + 
      geom_line() +
      labs(title = "Timeline of Injuries in the US",
           x = "Year", 
           y = "Number of Injuries") + theme_classic()
  }

  
  return(product)
}
