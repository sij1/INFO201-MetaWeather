library(shiny)
library(rsconnect)
library(dplyr)
library(jsonlite)
library(httr)

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
      year_data <- year_data,
      years_data = years_data
    )
    
  })
  
  
  
  output$table1 <- renderDataTable({
    data <- variables()[['years_data']][[1]]
  })
}  
)