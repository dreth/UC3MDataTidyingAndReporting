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
            ),

            # The correlation plots tab will calculate
            # several correlation coefficients and 
            # create a scatter plot between variables
            tabPanel("Correlation plots",
                sidebarLayout(
                    sidebarPanel(
                        radioButtons("radioCorrel",
                            label = h5("Select what type of plot to create"),
                            choices = c("Scatter plot","Correlation matrix", "Performance analytics"),
                            selected = 1
                        ),
                        selectInput("selectVar1Scatter",
                            label = h5("Select variable 1"),
                            choices = names(dem)[4:length(names(dem))-1],
                            selected = 1
                        ),
                        selectInput("selectVar2Scatter",
                            label = h5("Select variable 2"),
                            choices = names(dem)[4:length(names(dem))-1],
                            selected = 1
                        ),
                        checkboxInput("switchScatterVars",
                            label = h5("Switch Variables"),
                            value = 0
                        ),
                        h5("Correlation table for the selected variables"),
                        dataTableOutput(outputID = "correlTable")
                    ),
                    mainPanel(
                        plotOutput(outputId = "scatterPlotsCorrel")
                    ),
                )
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
            {plotHistBox()},
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

        # Correlation plots section, plot output
        output$scatterPlotsCorrel <- renderPlot({
            if (input$radioCorrel == "Scatter plot") {
                if (input$selectVar1Scatter != input$selectVar2Scatter) {
                    if (input$switchScatterVars == 1) {
                        ggplot(dem, aes(input$selectVar2Scatter, input$selectVar1Scatter))+
                            geom_point()+
                            ggtitle(str_interp("${input$selectVar2Scatter} vs ${input$selectVar1Scatter}"))
                    } else {
                        ggplot(dem, aes(input$selectVar1Scatter, input$selectVar2Scatter))+
                            geom_point()+
                            ggtitle(str_interp("${input$selectVar1Scatter} vs ${input$selectVar2Scatter}"))
                    }
                } else {
                    renderText({ "You must select different variables!"})
                }
            } else if (input$radioCorrel == "Correlation matrix") {
                # selecting numeric cols
                df <- dem[4:length(names(dem))-1]

                # This creates a correlation matrix with maximum
                # correlation from kendall, pearson and spearman
                # correlation coefficients
                methods = c('kendall','spearman','pearson')
                corr_mat = matrix(rep(0,(length(cols)^2)*4), nrow=length(cols)^2)
                corr_mat = corr_mat %>% data.frame() %>% setNames(c('var1','var2','coef','corr_type'))
                cnt = 0
                for (i in 1:length(cols)) {
                    for (j in 1:length(cols)) {
                        cnt = cnt + 1
                        comb1 <- df %>% dplyr::select(cols[i])
                        comb2 <- df %>% dplyr::select(cols[j])
                        maximum_cor = 0
                        method_used = ''
                        for (method in methods) {
                            correl <- cor(comb1[,1],comb2[,1], method=method)
                            if (abs(correl) > abs(maximum_cor)) {
                                maximum_cor <- correl
                                method_used = method
                            }
                        }
                        corr_mat$coef[cnt] = maximum_cor
                        corr_mat$var1[cnt] = cols[i]
                        corr_mat$var2[cnt] = cols[j]
                        corr_mat$corr_type[cnt] = method_used
                    }
                }

                # plotting the correlation heatmap
                ggplot(corr_mat, aes(var1, var2, fill=coef)) +
                 geom_tile() +
                 geom_text(aes(label=round(coef,2))) +
                 scale_fill_gradient(low="red", high="blue", limits=c(-1,1))+
                 theme( axis.text.x = element_text(angle = 70, vjust = 1, size = 12, hjust = 1),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank(),
                        axis.ticks = element_blank())
            } else if (input$radioCorrel == "Performance analytics") {
                # selecting numeric cols
                df <- dem[4:length(names(dem))-1]

                # plotting correlation chart with histogram and pearson corr coef
                chart.Correlation(df, histogram=TRUE, pch=19, method="pearson")
            }
        })
    }
)
