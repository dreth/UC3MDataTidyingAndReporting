# importing required libraries
require(shiny)
require(tidyverse)
require(shinyjs)
require(shinythemes)
require(dplyr)
require(ggplot2)
require(stringr)
require(PerformanceAnalytics)
require(foreach)
require(MASS)
require(gridExtra)

# importing data
dem <- read.csv('./data/data.csv')

# Function used to generate plots on the tab "histograms and boxplots"
# Recycling this cool function i coded from my final project of statistical learning
plots <- function(dataset, col, fw=FALSE, hist='default',
                  density='default' , bins='default',
                  xtick_angles='default', sep=FALSE, savefig='default', filename='./plot.png') {
    var <- dataset %>% dplyr::select(col)
    if (bins == 'default') {bins <- rep(10,2)}
    if (xtick_angles == 'default') {xtick_angles <- rep(90,2)}
    if (hist == 'default') {hist <- c(FALSE,FALSE)}
    if (density == 'default') {density <- c(TRUE,TRUE)}
    if (savefig == 'default') {savefig <- c(FALSE,14,14)}

    p1 <- dataset %>% ggplot(aes(x=var[,1])) +
        geom_boxplot() +
        ggtitle(str_interp("${col}")) +
        theme(axis.title.x=element_blank(),axis.text.y=element_blank())
    p2 <- dataset %>% ggplot(aes(x=var[,1], fill=hdi_cat)) +
        geom_boxplot() +
        ggtitle(str_interp("${col} grouped by HDI")) +
        theme(axis.title.x=element_blank(),axis.text.y=element_blank())
    p3 <- dataset %>% ggplot(aes(x=var[,1])) +
        ggtitle(str_interp("${col}")) +
        theme(axis.title.x=element_blank(),
                axis.text.x = element_text(angle = xtick_angles[1]))
    p4 <- dataset %>% ggplot(aes(x=var[,1])) +
        ggtitle(str_interp("${col} by HDI group")) +
        theme(axis.title.x=element_blank(),
                axis.text.x = element_text(angle = xtick_angles[2]))
    if (hist[1] == TRUE) {
        p3 <- p3 + geom_histogram(aes(y=..density..),bins=bins[1])}
    if (hist[2] == TRUE) {
        p4 <- p4 + geom_histogram(show.legend = FALSE,bins=bins[2],
                                  aes(fill=hdi_cat,y=..density..))}
    if (density[1] == TRUE) {
        p3 <- p3 + geom_density()}
    if (density[2] == TRUE) {
        p4 <- p4 + geom_density(aes(group=hdi_cat,colour=hdi_cat,fill=hdi_cat))}
    if (fw == TRUE) {p4 <- p4 + facet_wrap(~hdi_cat, nrow = 1)}
    if (sep == TRUE) {
        grid.arrange(p1,p2, nrow=2)
        grid.arrange(p3,p4, nrow=2)}
    else {grid.arrange(p1,p2,p3,p4, nrow=4)}
    if (savefig[1] == TRUE) {ggsave(file=filename, width=savefig[2], height=savefig[3],
                                 arrangeGrob(p1,p2,p3,p4, nrow=4))}
}


shinyApp(
    # UI of the application
    ui = navbarPage(theme = shinytheme("slate"), "Country metrics",
            # Introduction page
            tabPanel("Introduction",
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

            # Panel for histograms/boxplots
            # this panel uses the plots function defined
            # previously before the shiny app block
            tabPanel("Histograms/Boxplots",
                sidebarLayout(
                    sidebarPanel(
                        tabsetPanel(
                            tabPanel("Plot settings",
                                selectInput("selectVarHB",
                                    label = h5("Select variable to plot"),
                                    choices = names(dem)[4:length(names(dem))-1],
                                    selected = 1
                                ),
                                selectInput("plotHistHB1",
                                    label = h5("Plot histogram for variable itself"),
                                    choices = c(TRUE, FALSE)
                                ),
                                selectInput("plotHistHB2",
                                    label = h5("Plot histogram for variable by development"),
                                    choices = c(TRUE, FALSE)
                                ),
                                selectInput("plotDensHB1",
                                    label = h5("Plot Density curve for variable itself"),
                                    choices = c(TRUE, FALSE)
                                ),
                                selectInput("plotDensHB2",
                                    label = h5("Plot Density curve for variable by development"),
                                    choices = c(TRUE, FALSE)
                                ),
                                selectInput("fwHB",
                                    label = h5("Facet Wrap"),
                                    choices = c(TRUE, FALSE)
                                ),
                                numericInput("binsInputHB1",
                                    label="Number of bins for histogram of variable itself",
                                    value=20,
                                    min=5,
                                    max=100,
                                    step=1
                                ),
                                numericInput("binsInputHB2",
                                    label="Number of bins for histogram of variable by development",
                                    value=20,
                                    min=5,
                                    max=100,
                                    step=1
                                ),
                                numericInput("heightInputHB",
                                    label="Height of the image",
                                    value=930,
                                    min=800,
                                    max=3000,
                                    step=1
                                ),
                            ),
                            tabPanel("Save plot",
                                numericInput("widthInputSaveHB",
                                    label="Width of the image",
                                    value=8,
                                    min=1,
                                    max=30,
                                    step=1
                                ),
                                numericInput("heightInputSaveHB",
                                    label="Height of the image",
                                    value=8,
                                    min=1,
                                    max=30,
                                    step=1
                                ),
                                downloadButton("savePlotButtonHB",
                                    label="Save plot"
                                ),
                            )
                        ),
                        
                    ),
                    mainPanel(
                        plotOutput(outputId = "multiPlotsBasic")
                    ),
                ),
            )
        ),
    
    # Server function for the shiny app
    server = function(input, output) {
        # Plot function call for histogram/boxplot section
        plotHistBox = function() {
            suppressWarnings(
                plots(
                    dataset=dem, 
                    col=input$selectVarHB,
                    hist=c(input$plotHistHB1,input$plotHistHB2),
                    density=c(input$plotDensHB1,input$plotDensHB2),
                    xtick_angles=c(50,50),
                    bins=c(input$binsInputHB1,input$binsInputHB2),
                    fw=input$fwHB,
                    sep=FALSE
                )
            )
        }

        # Plot for the histogram/boxplot section
        output$multiPlotsBasic <- renderPlot(
        {
            plotHistBox()
        },
            width = "auto",
            height = reactive(input$heightInputHB)
        )

        # Download button for histogram/boxplot section
        output$savePlotButtonHB = downloadHandler(
            filename = str_interp("${input$selectVarHB}.png"),
            content = function(file) {
                ggsave(
                    file,
                    width=input$widthInputSaveHB,
                    height=input$heightInputSaveHB, 
                    plot=plotHistBox(),
                    device="png"
                )
            }
        )
    }
)
