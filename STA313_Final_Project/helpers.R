data = read.csv("finaldata_313.csv")
federal_laws = read.csv("federal_laws.csv")
federal_laws <- federal_laws[-(1:4), ]

#when adding filters, make sure to add filter parameters here
plot_timeline <- function(temp, temp_laws, response) {
  
  
  #get y values for law points
  points <- geom_point(data = temp_laws, aes(x = Year, y = count))
  
  #create graph
  product <- temp %>% 
    ggplot(aes(x = Year, y = count)) + geom_line() +
    labs(title= paste0("Timeline of ", response, " in the US"),
         x = "Year", 
         y = paste0("Number of ", response))

  product <- product + points + labs(subtitle = "Click on graph to 
                                     display policy information")
  
  return(product)
}

create_temps <- function(states, max, min, response) {
  
  #takes input from state, lists all states if option is checked
  if("All States" %in% states){
    states <- as.character(unique(data$State))
  } else {
    states <- states
  }
  
  #add any additional filters here
  temp <- data %>% filter(State %in% states) %>% group_by(Year)
  
  #looks at laws based on the filtered data
  temp_laws <- temp %>% filter(Year %in% federal_laws$Year_Implemented) %>% 
    group_by(Year)
  
  if(response == "Shootings"){
    temp <- temp %>% summarise(count = n())
    temp_laws <- temp_laws %>% summarise(count = n())
  } else if(response == "Fatalities"){
    temp <- temp %>% summarise(count = sum(Fatalities))
    temp_laws <- temp_laws %>% summarise(count = sum(Fatalities))
  } else {
    temp <- temp %>% summarise(count = sum(Injured))
    temp_laws <- temp_laws %>% summarise(count = sum(Injured))
  }
  
  
  #when you group by year, it excludes the data altogether
  #instead of placing a 0 for that year, here is my solution
  
  #this allows for the data points to be constant
  full_years <- tibble("Year" = unique(federal_laws$Year_Implemented), 
                       n = rep(0, length(unique(federal_laws$Year_Implemented)))) %>% 
    filter(Year < max) %>% filter(Year > min)
  temp_laws <- left_join(full_years, temp_laws, by = c("Year")) %>% mutate(
    count = case_when(is.na(count) ~ 0, 
                      TRUE ~ as.numeric(as.character(count))))
  
  #this allows for the count to be constant
  full <- tibble("Year" = c(1969:2019), n = rep(0, 51)) %>% 
    filter(Year < max) %>% filter(Year > min)
  temp <- left_join(full, temp, by = c("Year")) %>% mutate(
    count = case_when(is.na(count) ~ 0, 
                      TRUE ~ as.numeric(as.character(count))))
  
  return(list(temp, temp_laws))
  
}