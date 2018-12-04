library(jsonlite)
library(httr)
library(dplyr)

city <- "Seattle"
data <- GET(paste0("https://www.metaweather.com/api/location/search/?query=", city))
data_content <- fromJSON(content(data, "text"))
View(data_content)

## TODO:
## if else statement that chooses the number of days depending on the month
## Every Year from 2014
## lapply or other function that collects data for each day
## Collect them an plot
## Calculate likeliness/percentage

id <- data_content[1, 3]
date <- "2018/11/25"
uri <- paste0("https://www.metaweather.com/api/location/", id, "/", date)

city_data <- GET(uri)
city_content <- fromJSON(content(city_data, "text"))
View(city_content)

## Predicting future likeliness of weather state
## Wind speed and temperature change
## Temperature differences