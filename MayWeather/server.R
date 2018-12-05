library(shiny)
library(rsconnect)
library(dplyr)
library(jsonlite)
library(httr)
library(cowplot)
library(ggplot2)

shinyServer(
  function(input, output){
    
    variables <- reactive({
      city <- input$City
      date <- input$Date
      
      data <- GET(paste0("https://www.metaweather.com/api/location/search/?query=", gsub(" ", "&", city)))
      data_content <- fromJSON(content(data, "text")) 
      id <- data_content[1, 3]
      
      year <- 2018
      years <- c(2014 : 2018)
      
      year_funct <- function(year){
        date <- paste0(year, "/", date)
        uri <- paste0("https://www.metaweather.com/api/location/", id, "/", date)
        city_data <- GET(uri)
        city_content <- fromJSON(content(city_data, "text"))
        return(city_content)
      }
      
      year_data <- year_funct(year)
      years_data <- lapply(years, function(x) year_funct(x))
      
      list(
        city = city,
        date = date,
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
      View(minMax)
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
  }  
)