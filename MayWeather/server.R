library(shiny)
library(rsconnect)
library(dplyr)
library(jsonlite)
library(httr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggplot2)

shinyServer(
function(input, output){
  
  ## All the reactive variables used for the project
  variables <- reactive({
    if(input$City == "") {
      city <- "Seattle"
    } else {
      city <- input$City
    }
    
    if(input$Date == "") {
      date <- "11/25"
    } else {
      date <- input$Date
    }
    
    data <- GET(paste0("https://www.metaweather.com/api/location/search/?query=", gsub(" ", "&", city)))
    data_content <- fromJSON(content(data, "text")) 
    id <- data_content[1, 3]
    
    year <- 2018
    years <- c(2014 : 2018)
    var1 <- input$Var1
    var2 <- input$Var2
    
    year_funct <- function(year){
      date <- paste0(year, "/", date)
      uri <- paste0("https://www.metaweather.com/api/location/", id, "/", date)
      city_data <- GET(uri)
      city_content <- fromJSON(content(city_data, "text"))
      return(city_content)
    }
    
    year_data <- year_funct(year)
    years_data <- lapply(years, function(x) year_funct(x))
    
    ## Made a list to be able to access them in other functions
    list(
      city = city,
      date = date,
      year = year,
      years = years,
      var1 = var1,
      var2 = var2,
      year_data = year_data,
      years_data = years_data
    )
    
  })
  
  output$tempPlot <- renderPlot({
    city_data2 <- variables()[['year_data']]
    city <- variables()[['city']]
    date <- variables()[['date']]
    
    minMax <- city_data2 %>% 
      mutate(time = substr(created, 12, 19)) %>% 
      select(time, min_temp, max_temp) %>% 
      arrange(time) %>%
      mutate(time2 = as.numeric(substr(time, 1, 2)) +
               (as.numeric(substr(time, 4, 5)) / 60) +
               (as.numeric(substr(time, 7, 8)) / 3600)) 
    minTemp <- minMax %>% 
      filter(min_temp == min(min_temp)) 
    maxTemp <- minMax %>% 
      filter(max_temp == max(max_temp))
    
    ggplot(minMax, aes(x = time2, width = .75)) + 
      geom_bar(aes(y = max_temp),stat = "identity", fill = "red", position = "dodge") +
      geom_bar(aes(y = min_temp),stat = "identity", fill = "blue", position = "dodge") +
      scale_x_continuous(breaks = c(0:24)) +
      ylab("Temperature") +
      xlab("Time") +
      labs(title = paste("Max/Min Temperature of", city, "on the day of", date))
  })
  
  output$output <- renderText({
    
    city_data2 <- variables()[['year_data']]
    city <- variables()[['city']]
    date <- variables()[['date']]
    
    minMax <- city_data2 %>% 
      mutate(time = substr(created, 12, 19)) %>% 
      select(time, min_temp, max_temp) %>% 
      arrange(time) 
    
    minTemp <- minMax %>% 
      filter(min_temp == min(min_temp)) 
    maxTemp <- minMax %>% 
      filter(max_temp == max(max_temp))
    
    paste("On", date, " in", city,", the maximum temperature was", round(maxTemp[1,3], 2),
          "and the minimum temperature was", round(minTemp[1,2],2),".")
  })
  
  ## Reactive function that creates the necessary table for plotting the state plot
  weatherTable <- reactive({
    years <- variables()[['years']]
    years_data <- variables()[['years_data']]
    
    weather_funct <- function(data){
      weather_data <- data %>% 
                      select(weather_state_name) %>% 
                      group_by(weather_state_name) %>% 
                      summarize(n = n()) %>% 
                      ungroup() %>% 
                      mutate(probability = (n / sum(n)))
      return(weather_data)
    }
    weather_data <- lapply(years_data, function(x) weather_funct(x))
  })
  
  ## Reactive function that creates the necessary table for plotting the correlation plot
  correlation <- reactive({
    var1 <- variables()[['var1']]
    var2 <- variables()[['var2']]
    cor_data <- variables()[['years_data']] %>% 
                  plyr::ldply(., data.frame) %>% 
                  select(UQ(sym(var1)), UQ(sym(var2)))
    return(cor_data)
  })
  
  ## Plots the correlation using the past five year of data
  output$corplot <- renderPlot({
    var1 <- variables()[['var1']]
    var2 <- variables()[['var2']]
    city <- variables()[['city']]
    date <- variables()[['date']]
    
    cor_data <- correlation()
    ggplot(data = cor_data, aes(x = UQ(sym(var1)), y = UQ(sym(var2)))) +
      geom_point() +
      stat_smooth(method = "lm", formula = y ~ x) +
      xlab(gsub("_", " ", variables()[['var1']])) +
      ylab(gsub("_", " ", variables()[['var2']])) +
      ggtitle(paste0("Correlation Plot of ", gsub("_", " ", variables()[['var1']]), " and ", gsub("_", " ", variables()[['var2']]), " in ", 
                     city, " on date ", date)) +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  
  ## Plots the states and its probability using the previously occured weather states the past five years
  output$weatherplot <- renderPlot({
    years <- variables()[['years']]
    city <- variables()[['city']]
    date <- variables()[['date']]
    
    weather_data <- weatherTable() %>% 
                    plyr::ldply(., data.frame) %>% 
                    aggregate(cbind(probability) ~ weather_state_name, data = ., FUN = sum) %>% 
                    mutate(probability = round(probability / length(years), 4) * 100)              

    ggplot(data = weather_data, aes(x = weather_state_name, y = probability)) +
      geom_bar(stat = "identity", color = "black", fill = "blue") +
      geom_text(aes(label = probability), vjust = -0.5, size = 5.0) +
      ggtitle(paste0("Probability of Likely Weather Status at ", city, " on Date ", date, "(mm/dd)")) +
      ylab("Probability(%)") +
      xlab("Likely Weather States") +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  
  ## This explanation explains the user about the weather state plot
  output$wexplanation <- renderText({
    date <- variables()[['date']]
    paste0("The following barplot represents the probability of each weather state for a specific date, using the data from the past five years on ", date)
  })
  
  ## This explanation explains the user about the correlation plot
  output$cexplanation <- renderText({
    var1 <- gsub("_", " ", variables()[['var1']])
    var2 <- gsub("_", " ", variables()[['var2']])
    date <- variables()[['date']]
    
    paste0("The following plot represents a correlation between the variables selected by the user. The data is represented by using the past five years of data for ", date, ". The following plot uses ", var1, " as the x values and ", var2, " as y values.")
  })
  
  ## Introduction statement for the project
  output$introduction <- renderText({
    "For our final project, we decided to use the MetaWeather API dataset. The dataset is accesible with their API uri, which requires a specific city ID, which can be obtained by searching their location API dataset. The main dataset contains data about time, min and max temperature, wind speed, wind direction, air pressure, humidity, visibility, and predictability. This application can be used to find the probabilty of a weather state on a date, observe the pattern in maximum and minimum temperature throughout the day, and look at correlation of any of the data the user desires about the date. The dataset requires the city name and date to be in exact format. Our application is aimed at people who need a basic idea of how the weather is for the city they are going to (or plan on going to)."
  })
  
  ## URL to the MetaWeather api page
  output$url <- renderUI({
    url <- a("MetaWeather API Link", href = "https://www.metaweather.com/api/")
  })
  
  ## Caution for the text inputs for users.
  output$caution <- renderText({
    "CAUTION: If the input isn't typed in correctly (spacing problem, date incorrectly put in) the plot will produce an error. Some cities will not work"
  })
  
  output$corCaution <- renderText({
    "Select two inputs to check if they are correlated. The following pair of inputs may not have a correlation at all"
  })
}  
)