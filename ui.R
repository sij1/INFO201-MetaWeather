# A ui.R file, that drives the structure of the user interface

library(shiny)

# Defines a fluidPage
my_ui <- fluidPage(
  
  # Creates a Shiny UI that contains an title, sidebar, and main panel
  pageWithSidebar(
    
    # Defines application title.
    headerPanel("Temperature Differences by City"),
    
    # Creates two widgets in the sidebar
    sidebarPanel(
      textInput("City", "Please enter a city (ex. Seattle)  :"),
      textInput("Date", "Please enter a date (ex. ??/??) :")
    ),
    
    # puts everything on output
    mainPanel(
      plotOutput("tempPlot"),
      textOutput("output")
    )
  )
)