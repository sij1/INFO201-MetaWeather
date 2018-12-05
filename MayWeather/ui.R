library(shiny)
library(rsconnect)
library(dplyr)
library(jsonlite)

## TODO
## READ RUBRIC
## Better description
## Labels, Axis, Units
## Comment on Code

## Possible correlations
## Wind speed and air pressure
## Temperature and Humidity



shinyUI(
  fluidPage(
    titlePanel("MetaWeather Project"),
    htmlOutput("url"),
    textOutput("introduction"),
    
    sidebarPanel(
      textOutput("caution"),
      textInput("City", "Please Input Your City"),
      textInput("Date", "Please Input the Desired Date (mm/dd)"),
      textOutput("corCaution"),
      selectInput("Var1", "Please Input Your Variable 1",
                  choices = c("min_temp", "max_temp", "the_temp", "wind_speed", "air_pressure", "humidity", "visibility")),
      selectInput("Var2", "Please Input Your Variable 2",
                  choices = c("min_temp", "max_temp", "the_temp", "wind_speed", "air_pressure", "humidity", "visibility"))
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Weather State", textOutput("wexplanation"), plotOutput("weatherplot")),
                  tabPanel("Correlation", textOutput("cexplanation"), plotOutput("corplot"))
      )
    )
  )
)