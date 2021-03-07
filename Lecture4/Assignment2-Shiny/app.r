require(shiny)
require(tidyverse)
require(shinyjs)
library(Stat2Data)

# Define UI for app that draws a histogram ----
ui <- navbarPage("First shiny app",
        tabPanel("Histogram per cancer",
            sidebarLayout(
                sidebarPanel(
                    selectInput("selectType",
                    label = h4("Select Cancer type"),
                    choices = unique(CancerSurvival$Organ),
                    selected = 1)
                ),
                mainPanel(
                    plotOutput(outputId = "histogram")
                )
            )
        ),
        tabPanel("Boxplot per cancer",
            sidebarLayout(
                sidebarPanel(
                    selectInput("selectTypeB",
                    label = h4("Select Cancer type"),
                    choices = unique(CancerSurvival$Organ),
                    selected = 1)
                ),
                mainPanel(
                    plotOutput(outputId = "boxplot")
                )
            )
        )
    )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    output$histogram <- renderPlot({
        if (input$selectType != ""){
            cs <- CancerSurvival %>% dplyr::filter(Organ == input$selectType) %>% dplyr::select(Survival) %>% unlist()
            hist(cs, breaks=5)
        }
    });

    output$boxplot <- renderPlot({
        if (input$selectTypeB != ""){
            cs <- CancerSurvival %>% dplyr::filter(Organ == input$selectTypeB) %>% dplyr::select(Survival) %>% unlist()
            boxplot(cs, horizontal=TRUE)
        }
    });
}

# Run the application 
shinyApp(ui = ui, server = server)

```{r include=FALSE}
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(magrittr)
library(tableHTML)
```
```{r include=FALSE}
datosA=read.csv("./DatosNBA.csv", sep=";", dec=".")
```
```{r include=FALSE}
datos=datosA[,2:30]
datos %<>% mutate_at(c("Player", "Pos","Age","Tm"), as.factor)
datos_Player = levels(datos$Player) %>% str_sort()
datos_Pos = levels(datos$Pos) %>% str_sort()
datos_Age = levels(datos$Age) %>% str_sort()
datos_Tm = levels(datos$Tm) %>% str_sort()
```
