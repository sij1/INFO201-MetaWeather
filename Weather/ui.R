library(shiny)
library(rsconnect)
library(dplyr)
library(jsonlite)

## TODO
## READ RUBRIC
## Write a summary, what question does it answer? what dataset is being used?
## Better description
## Extra data interaction?
## Labels, Axis, Units



shinyUI(
  fluidPage(
    titlePanel("Weather State Likelihood Probability"),

    sidebarPanel(
      textInput("City", "Please Input Your City"),
      textInput("Date", "Please Input the Desired Date (mm/dd)"),
      selectInput("Var1", "Please Input Your Variable 1",
                  choices = c("min_temp", "max_temp", "the_temp", "wind_speed", "air_pressure", "humidity", "visibility")),
      selectInput("Var2", "Please Input Your Variable 2",
                  choices = c("min_temp", "max_temp", "the_temp", "wind_speed", "air_pressure", "humidity", "visibility"))
    ),
    
    mainPanel(
      htmlOutput("uri"),
      textOutput("introduction"),
      tabsetPanel(type = "tabs",
                  tabPanel("Weather State", textOutput("wexplanation"), plotOutput("weatherplot")),
                  tabPanel("Correlation", textOutput("cexplanation"), plotOutput("corplot"))
      )
    )
  )
)