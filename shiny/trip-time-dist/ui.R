library(shiny)
library(plotly)


shinyUI(fluidPage(
  titlePanel("Distribution of Trip Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('type', 'Type of Membership',
                  choices=c('Subscriber', 'Customer'),
                  selected='Subscriber'),
      
      selectInput('city', 'City', 
                  choices=c("San Francisco", 
                            "Redwood City", 
                            "Palo Alto", 
                            "Mountain View", 
                            "San Jose"), 
                  selected="San Francisco"),
      
      selectInput('start', 'Start Station', 
                  choices=getStationLabel("San Francisco")),
      
      selectInput('end', 'End Station', 
                  choices=getStationLabel("San Francisco")),
      
      helpText("This application has been developed to support",
               tags$a(href='http://www.bayareabikeshare.com/open-data', 
                      "Bay Area Bike Share data analysis."),
               "It shows a distribution of trip time for",
               "a specific route by each type of membership."),
      
      helpText("Note, only data from March to August in 2014 is used.")
    ),
    
    mainPanel(
      plotlyOutput("plot")
    )
  )
))
