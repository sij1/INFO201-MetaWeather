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
    date <- paste0("2018/", date)
    uri <- paste0("https://www.metaweather.com/api/location/", id, "/", date)
    city_data <- GET(uri)
    city_content <- fromJSON(content(city_data, "text"))
    View(city_content)
    
    list(
      city = city,
      date = date,
      city_content = city_content
    )
    
  })
  
  output$table1 <- renderDataTable({
    data <- variables()[['city_content']]
  })
}  
)