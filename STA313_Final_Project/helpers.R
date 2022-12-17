data = read.csv("finaldata_313.csv")
federal_laws = read.csv("federal_laws.csv")
federal_laws <- federal_laws[-(1:4), ]
policies <- read.csv("policies.csv")

#when adding filters, make sure to add filter parameters here
plot_timeline <- function(temp, temp_laws, response) {
  
  
  #get y values for law points
  points <- geom_point(data = temp_laws, aes(x = Year, y = count),
                       color = "red", size = 3)
  
  #add in vertical lines
  lines <- geom_vline(xintercept = temp_laws$Year, color = "red", linetype =
                        "dashed", linewidth = 0.5)
  
  # #add the images in, little messy?
  # images <- geom_image(data = temp_laws, aes(x = Year, 
  #                                            y = count, image = "policy.png"), 
  #                      size = .075)
  
  #create graph
  product <- temp %>% 
    ggplot(aes(x = Year, y = count)) + geom_line(linewidth = 1) +
    labs(title= paste0("Timeline of ", response, " in the US"),
         x = "Year", 
         y = paste0("Number of ", response))

  product <- product + points + lines + theme_grey() +
    labs(subtitle = "Each point corresponds to a federal level policy. Click on a point to display the policy's information.",
         caption = "Our dataset includes a non-comprehensive list of US Mass Shootings from 1969-2019.") +
    theme(plot.subtitle = element_text(hjust = 0, size = 13),
          plot.title = element_text(face = "bold", hjust = 0, size = 20),
          plot.caption = element_text(face = "italic", hjust = 0, size = 12), 
          axis.title = element_text(face = "bold", size=12),
          axis.text = element_text(size=12))
  
  if(sum(temp$count) == 0){
    product <- product + geom_label(
      label = "No data points found based on filters.", 
      aes(x = mean(c(min(Year), max(Year))), y = 0), size = 5)
  }
  

  return(product)
}


create_temps <- function(states, max, min, response, type, motivation, 
                         keyword, mhhistory, sex, weapon) {
  
  #takes input from state, lists all states if option is checked
  if("All States" %in% states){
    states <- as.character(unique(data$State))
  }
  
  #takes input from types, lists all if option is checked
  if(type =="All Types"){
    type <- as.character(unique(data$Shooting.Type))
  } else {
    type <- switch(type, "Mass (Single Location)" = "Mass",
                   "Spree (Multiple Locations)" = "Spree",
                   "Unknown (Missing Information)" = NA)
  }
  
  #takes input from motivation, lists all if option is checked
  if(motivation =="All Motivations"){
    motivation <- as.character(unique(data$Cause))
  }
  if(is.null(mhhistory)){
    mhhistory <- as.character(unique(data$Prior.MH))
  }
  if(length(weapon) > 1){
    weapon <- paste0(weapon, collapse = "|")
  } else if(is.null(weapon)){
    weapon <- "NULL"
  }

  #add any additional filters here
  temp <- data %>% filter(State %in% states, Shooting.Type %in% type, 
                          Cause %in% motivation, 
                          grepl(weapon, str_to_lower(Weapon.Type)),
                          str_detect(str_to_lower(Summary),
                                     str_to_lower(keyword)) | str_detect(str_to_lower(MH.Details),str_to_lower(keyword)),
                          Prior.MH %in% mhhistory,
                          Gender %in% sex) %>%
    group_by(Year)
  


  
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

