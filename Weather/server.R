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
  
  correlation <- reactive({
    var1 <- variables()[['var1']]
    var2 <- variables()[['var2']]
    cor_data <- variables()[['years_data']] %>% 
                  plyr::ldply(., data.frame) %>% 
                  select(UQ(sym(var1)), UQ(sym(var2)))
    return(cor_data)
  })
  
  
  output$corplot <- renderPlot({
    var1 <- variables()[['var1']]
    var2 <- variables()[['var2']]
    cor_data <- correlation()
    ggplot(data = cor_data, aes(x = UQ(sym(var1)), y = UQ(sym(var2)))) +
      geom_point() +
      stat_smooth(method = "lm", formula = y ~ x) +
      xlab(gsub("_", " ", variables()[['var1']])) +
      ylab(gsub("_", " ", variables()[['var2']])) +
      ggtitle("Correlation Plot by User's Choice") +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  
  
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
  
  output$wexplanation <- renderText({
    "The following barplot represents the probability of each weather state for a specific date, using the data from the past five years as the background"
  })
  
  output$cexplanation <- renderText({
    var1 <- gsub("_", " ", variables()[['var1']])
    var2 <- gsub("_", " ", variables()[['var2']])
    
    paste0("The following plot represents a correlation between the variables selected by the user. The following plot uses ", var1, " as the x values and ", var2, " as y values.")
  })
  
  output$introduction <- renderText({
    "For our final project, we decided to use the MetaWeather API dataset. The dataset is accesible with their API uri, which requires a speicific city ID, which can be obtained by searching for it on the API. The dataset contains data about time, min and max temperature, wind speed, wind direction, air pressure, humidity, visibility, and predictability. This application can be used to find the probabilty of a weather state on a date, observe the pattern in maximum and minimum temperature throughout the day, and look at correlation of any of the data the user desires about the date. The dataset requires the city name and date to be in exact format."
  })
  
  output$uri <- renderUI({
    url <- a("MetaWeather API Link", href = "https://www.metaweather.com/api/")
  })
  
}  
)