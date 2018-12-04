library(shiny)
library(rsconnect)
library(dplyr)
library(jsonlite)
library(httr)
library(tidyr)
library(ggplot2)

shinyServer(
function(input, output){
  
  variables <- reactive({
    if(input$City == "") {
      city <- "Seattle"
    } else {
      city <- input$City
    }
    
    if(input$Date == "") {
      date <- "11/25"
    } else {
      city <- input$City
    }
    
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
      year = year,
      years = years,
      year_data <- year_data,
      years_data = years_data
    )
    
  })
  
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
  
  output$myplot <- renderPlot({
    years <- variables()[['years']]
    weather_data <- weatherTable() %>% 
                    plyr::ldply(., data.frame) %>% 
                    aggregate(cbind(probability) ~ weather_state_name, data = ., FUN = sum) %>% 
                    mutate(probability = probability / length(years))              

    ggplot(data = weather_data, aes(x = weather_state_name, y = probability)) +
      geom_bar(stat = "identity")
  })
  
}  
)