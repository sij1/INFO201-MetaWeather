library(shiny)
library(rsconnect)
library(dplyr)
library(jsonlite)

shinyUI(
  pageWithSidebar(
    headerPanel("Weather State Likeliness"),
    
    sidebarPanel(
      textInput("City", "Please Input Your City"),
      textInput("Date", "Please Input the Desired Date (mm/dd)")
    ),
    
    mainPanel(
      plotOutput("myplot")
    )
  )
)