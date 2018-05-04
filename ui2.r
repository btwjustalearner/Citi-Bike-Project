
library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Final Project"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      
      sliderInput(inputId = "day",
                  label = "Day",
                  min = 1,
                  max = 31,
                  value = 1),
      
      sliderInput(inputId = "hour",
                  label = "Hour",
                  min = 1,
                  max = 23,
                  value = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("summary",plotOutput("distPlot")),
        tabPanel("popular",plotOutput("popularplot")),
        tabPanel("suggestions",
                 plotOutput("plot1"),
                 plotOutput("plot2"),
                 plotOutput("plot3"),
                 plotOutput("plot4"),
                 plotOutput("plot5")
        )
      )
      
      
      
    )
  )
)

