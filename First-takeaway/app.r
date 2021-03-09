require(shiny)
require(tidyverse)
require(shinyjs)
require(shinythemes)

dem <- read.csv('./data/data.csv')

shinyApp(
    # UI of the application
    ui = navbarPage(theme = shinytheme("slate"), "Country metrics",
            # Introduction page
            tabPanel("Introduction",
                # Custom CSS
                tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                ),
                fluidPage(id="introductionPage",
                    h1("Country metrics"),
                    h3("A set of visualizations for different worldwide demographic/development metrics"),
                    p("Obtained from the",
                        a("World bank Databank",href="https://databank.worldbank.org/home.aspx"),
                        "specifically the",
                        a("World Development Indicators database",href="https://databank.worldbank.org/source/world-development-indicators"),
                        "This is the 'primary World Bank collection of development indicators' as stated on the database description. It has lots of economic, education, energy use, and population specific metrics."),
                    p("The dataset used (as it was used) can be found in the file tree of the repository for this shiny app. the file is easily identifiable within a folder called 'data' and named 'data.csv'."),
                    br(),
                    img(src="https://raw.githubusercontent.com/dreth/UC3MDataTidyingAndReporting/main/First-takeaway/www/worldbanklogo.png", width=462.222, height=260)
                ),
                
            ),
            # 
            tabPanel("",
            
            )
        ),

    # Server function for the shiny app
    server = function(input, output) {
        
    }
)
