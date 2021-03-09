require(shiny)
require(tidyverse)
require(shinyjs)

dem <- read.csv('./data.csv')

# Define UI for app that draws a histogram ----
ui <- navbarPage("Country metrics",
        tabPanel("",
            
        ),
    )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)