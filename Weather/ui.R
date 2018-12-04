library(shiny)
library(rsconnect)
library(dplyr)
library(jsonlite)

shinyUI(
  pageWithSidebar(
    headerPanel("Weather State Likelihood Probability"),
    
    sidebarPanel(
      textInput("City", "Please Input Your City"),
      textInput("Date", "Please Input the Desired Date (mm/dd)")
    ),
    
    mainPanel(
      textOutput("explanation"),
      plotOutput("myplot")
    )
  )
)