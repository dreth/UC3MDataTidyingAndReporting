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
require(plotly)

# importing data
dem <- read.csv('https://raw.githubusercontent.com/dreth/UC3MStatisticalLearning/main/data/without_tags/data.csv')

# numeric columns
cols <- names(dem)[4:length(names(dem))-1]

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
          h4("Dataset variables:"),
          tags$ul(
            tags$li(strong("year_code"), ": code for the year as the world bank databank sets it"),
            tags$li(strong("country_name"), ": name of the country"),
            tags$li(strong("country_code"), ": alpha-3 ISO 3166 code for the country"),
            tags$li(strong("foreign_inv_inflows"), ": Foreign direct investment, net inflows (BoP, current US$)"),
            tags$li(strong("exports_perc_gdp"), ": Exports of goods and services (as a % of GDP)"),
            tags$li(strong("inflation_perc"), ": Inflation, consumer prices (annual %)"),
            tags$li(strong("education_years"), ": Compulsory education, duration (years)"),
            tags$li(strong("education_perc_gdp"), ": Government expenditure on education, total (as a % of GDP)"),
            tags$li(strong("gds_perc_gdp"), ": Gross domestic savings (as a % of GDP)"),
            tags$li(strong("gross_savings_perc_gdp"), ": Gross savings (as a % of GDP)"),
            tags$li(strong("int_tourism_arrivals"), ": International tourism, number of arrivals"),
            tags$li(strong("int_tourism_receipts"), ": International tourism, receipts (in current US$)"),
            tags$li(strong("perc_internet_users"), ": Individuals using the Internet (as a % of population)"),
            tags$li(strong("access_to_electricity"), ": Access to electricity (% of population)"),
            tags$li(strong("agricultural_land"), ": Agricultural land (% of land area)"),
            tags$li(strong("birth_rate"), ": Birth rate, crude (per 1,000 people)"),
            tags$li(strong("gne"), ": Gross national expenditure (% of GDP)"),
            tags$li(strong("mobile_subscriptions"), ": Mobile cellular subscriptions (per 100 people)"),
            tags$li(strong("infant_mort_rate"), ": Mortality rate, infant (per 1,000 live births)"),
            tags$li(strong("sex_ratio"), ": Sex ratio at birth (male births per female births)"),
            tags$li(strong("greenhouse_gas_em"), ": Total greenhouse gas emissions (kt of CO2 equivalent)"),
            tags$li(strong("urban_pop_perc"), ": Urban population (% of total population)"),
            tags$li(strong("hdi"), ": human development index "),
            tags$li(strong("hdi_cat"), ": Human development index as a category"),
            tags$li(strong("life_exp"), ": Life expectancy at birth, total (years)"),
            tags$li(strong("gdp"), ": GDP (current US$) "),
            tags$li(strong("gni"), ": GNI (current US$)"),
            tags$li(strong("fertility_rate"), ": Fertility rate, total (births per woman)")
          ),
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
                  label = h4("Select variable to plot"),
                  choices = cols,
                  selected = 1
                ),
                selectInput("plotHistHB1",
                  label = h4("Plot histogram for variable itself"),
                  choices = c(TRUE, FALSE)
                ),
                selectInput("plotHistHB2",
                  label = h4("Plot histogram for variable by development"),
                  choices = c(TRUE, FALSE)
                ),
                selectInput("plotDensHB1",
                  label = h4("Plot Density curve for variable itself"),
                  choices = c(TRUE, FALSE)
                ),
                selectInput("plotDensHB2",
                  label = h4("Plot Density curve for variable by development"),
                  choices = c(TRUE, FALSE)
                ),
                selectInput("fwHB",
                  label = h4("Facet Wrap"),
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
              label = h4("Select what type of plot to create"),
              choices = c("Scatter plot","Correlation matrix", "Performance analytics"),
              selected = "Scatter plot"
            ),
            selectInput("selectVar1Scatter",
              label = h4("Select variable 1"),
              choices = cols,
              selected = sample(cols,1)
            ),
            selectInput("selectVar2Scatter",
              label = h4("Select variable 2"),
              choices = cols,
              selected = sample(cols,1)
            ),
            checkboxInput("switchScatterVars",
              label = "Switch Variables",
              value = 0
            ),
            checkboxInput("scatterGroupByHDI",
              label = "Color by HDI",
              value = 1
            ),
            strong(h4("Correlation table for the selected variables")),
            tableOutput(outputId = "correlTable")
          ),
          mainPanel(
            plotOutput(outputId = "scatterPlotsCorrel")
          ),
        )
      ),

      # Interactive plots (plotly)
      tabPanel("Aggregates per HDI",
        sidebarLayout(
          sidebarPanel(
            selectInput("plotlyVarSelect",
              label = h4("Select variable to plot"),
              choices = cols,
              selected = sample(cols,1)
            ),
            selectInput("plotlyFUNSelect",
              label = h4("Select function to aggregate"),
              choices = c("mean","median","min","max"),
              selected = sample(c("mean","median","min","max"),1)
            )
          ),
          mainPanel(
            plotly::plotlyOutput(outputId = "plotlyInteractivePlot")
          ),
        )
      ),


      # Top N countries (selected by the user)
      # this section includes a report on these
      tabPanel("Top n countries",
        sidebarLayout(
          sidebarPanel(
            tabsetPanel(
              # information on this section
              tabPanel("Information",
                p("This section will allow you to create plots representative of the top (or bottom) countries per each development variable."),
                p("After a complete selection, a report will be created which details which countries are in the top and their respective HDI category.")
              ),

              # Plot settings
              tabPanel("Plot parameters",
                selectizeInput("selectVarsTop",
                  label = h4("Select variables to plot"),
                  choices = cols,
                  options = list(maxItems = length(cols))
                ),
                selectInput("selectIdentityTop",
                  label = h4("Use country code or country name for x-axis"),
                  choices = c("country_name", "country_code"),
                  selected = "country_name"
                ),
                numericInput("selectTopN",
                  label="How many top/bottom countries to plot",
                  value=10,
                  min=1,
                  max=length(dem$country_name),
                  step=1
                ),
                radioButtons("radioTopOrBottom",
                  label = h4("Plot top or bottom N countries"),
                  choices = c("Top","Bottom"),
                  selected = "Top"
                ),
                checkboxInput("markHDICategory",
                  label = "Color by HDI",
                  value = 0
                )
              ),

              # Report parameters
              tabPanel("Report parameters",
                h4("Report parameter selection"),
                p("Select what metrics and variables to include in the top N report"),
                checkboxGroupInput("checkReportParameters",
                  label = h4("Metric selection"),
                  choices = c("Mean", "Median", "Max", "Min"),
                  selected = c("Mean", "Median", "Max", "Min")
                ),
                radioButtons("outputFileFormat",
                  label = h4("Select output file format"),
                  choices = c("PDF", "HTML", "Word"),
                  selected = "HTML",
                  inline = TRUE
                ),
                downloadButton("downloadReport",
                  label="Generate report"
                )
              )
            )
          ),
          mainPanel(
            plotOutput(outputId = "topNPlot")
          )
        )
      )
    ),
  
  # Server function for the shiny app
  server = function(input, output, session) {

    # %% HELPER FUNCTIONS %%
    # Function used to generate plots on the tab "histograms and boxplots"
    # Recycling this cool function i coded from my final project of statistical learning
    plots <- function(dataset, col, fw=FALSE, hist='default',
            density='default' , bins='default',
            xtick_angles='default') {
      var <- dataset %>% dplyr::select(col)
      if (bins == 'default') {bins <- rep(10,2)}
      if (xtick_angles == 'default') {xtick_angles <- rep(90,2)}
      if (hist == 'default') {hist <- c(FALSE,FALSE)}
      if (density == 'default') {density <- c(TRUE,TRUE)}

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
      grid.arrange(p1,p2,p3,p4, nrow=4)
    }

    # Function to plot correlations by group or individually for 2 vars
    # along with performance analytics chart correlation plot
    correl_plots <- function(dataset, inputs, group=TRUE, invert=FALSE) {
      scatterP <- ggplot(dataset)+
              theme(axis.title.x = element_blank(),
                axis.title.y = element_blank())
      if (invert == TRUE) {inputs <- rev(inputs)}
      if (group == TRUE) {
        result <- scatterP+
              geom_point(aes_string(inputs[1], inputs[2], color="hdi_cat"))+
              ggtitle(str_interp("${inputs[1]} vs ${inputs[2]} by HDI group"))
      } else {
        result <- scatterP+
          geom_point(aes_string(inputs[1], inputs[2]))+
          ggtitle(str_interp("${inputs[1]} vs ${inputs[2]}"))
      }
      result
    }

    # Function to plot barplots for the top N countries tab
    topn_barplot <- function(dataset, identity, vars, orient, amount, cat) {
      if (length(vars) == 1) {
        if (orient == "Top") {
            demTopN <- arrange_at(dem, vars[1], 'desc')
            demTopN[,identity] <-  factor(demTopN[,identity], levels=demTopN[,identity])
          } else {
            demTopN <- arrange_at(dem, vars[1])
            demTopN[,identity] <-  factor(demTopN[,identity], levels=demTopN[,identity])
          }
        plt <- ggplot(demTopN[1:amount,], aes_string(x=identity, y=vars[1]))+
          ggtitle(str_interp("${orient} ${amount}: ${vars[1]}"))+
          theme(axis.text.x = element_text(angle = 45))
        if (cat == 1) {
          plt + geom_bar(stat="identity", aes_string(fill="hdi_cat"))
        } else {
          plt + geom_bar(stat="identity")
        }
      } else {
        plts <- list()
        for (i in 1:length(vars)) {
          if (orient == "Top") {
            demTopN <- arrange_at(dem, vars[i], 'desc')
            demTopN[,identity] <-  factor(demTopN[,identity], levels=demTopN[,identity])
          } else {
            demTopN <- arrange_at(dem, vars[i])
            demTopN[,identity] <-  factor(demTopN[,identity], levels=demTopN[,identity])
          }
          plts[[i]] = ggplot(demTopN[1:amount,], aes_string(x=identity, y=vars[i]))+
            ggtitle(str_interp("${orient} ${amount}: ${vars[i]}"))+
            theme(axis.text.x = element_text(angle = 45))
          if (cat == 1) {
            plts[[i]] <- plts[[i]] + geom_bar(stat="identity", aes_string(fill="hdi_cat"))
          } else {
            plts[[i]] <- plts[[i]] + geom_bar(stat="identity")
          }
        }
        grid.arrange(grobs=plts, nrow=length(vars))
      }
    }



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
          fw=input$fwHB
        )
      )
    }
    
    # %% OUTPUTS %%
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
        # correl inputs vector to simplify notation
        correlInputs <- c(input$selectVar1Scatter, 
                  input$selectVar2Scatter)
        if (correlInputs[1] != correlInputs[2]) {
          if (input$switchScatterVars == 1) {
            if (input$scatterGroupByHDI == 1) {
              # scatter plot var 2 vs var 1 grouped by HDI
              correl_plots(dem, inputs=correlInputs,
                     group=TRUE, invert=TRUE)
            } else {
              # scatter plot var 2 vs var 1 not grouped
              correl_plots(dem, inputs=correlInputs,
                     group=FALSE, invert=TRUE)
            }
          } else {
            if (input$scatterGroupByHDI == 1) {
              # scatter plot var 1 vs var 2 grouped by HDI
              correl_plots(dem, inputs=correlInputs,
                     group=TRUE, invert=FALSE)
            } else {
              # scatter plot var 1 vs var 2 not grouped
              correl_plots(dem, inputs=correlInputs,
                     group=FALSE, invert=FALSE)
            }
          }
        } else {
          text(x=0.5, y=0.5, col="black", cex=2, "You must select different variables!")
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
         scale_fill_gradient(low="red", high="blue", limits=c(-1,1))+
         theme(axis.text.x = element_text(angle = 70, vjust = 1, size = 9, hjust = 1),
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
    },
      # match width for a square plot
      height = function () {
        session$clientData$output_scatterPlotsCorrel_width
      }
    )

    # data table with correlations of the 2 variables selected
    # in the scatter plot section
    output$correlTable <- renderTable({
      correlCoefs <- c('kendall','spearman','pearson')
      correlValues <- rep(0,length(correlCoefs))
      for (i in 1:length(correlCoefs)) {
        correlV1 <- dem %>% dplyr::select(input$selectVar1Scatter) %>% unlist()
        correlV2 <- dem %>% dplyr::select(input$selectVar2Scatter) %>% unlist()
        correlValues[i] <- cor(correlV1,correlV2, method=correlCoefs[i])
      }
      data.frame(method=correlCoefs, coefficient=correlValues)
    })

    # Plot for top N countries
    output$topNPlot <- renderPlot({
      topn_barplot(dataset=dem,
             identity=input$selectIdentityTop,
             vars=input$selectVarsTop,
             orient=input$radioTopOrBottom,
             amount=input$selectTopN,
             cat=input$markHDICategory)
    },
      height = function () {
        if (length(input$selectVarsTop) > 1) {
          session$clientData$output_topNPlot_width * (1 + round((3/7)*log(length(input$selectVarsTop))))
        } else {
          "auto"
        } 
      }
    )

    # Generate report for top N countries
    output$downloadReport <- downloadHandler(
      # Extension selector
      filename = function() {
        ext <- switch(input$outputFileFormat, PDF = 'pdf', HTML = 'html', Word = 'docx')
        str_interp('topNreport.${ext}')
      },
      # content of the report
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.rmd")
        file.copy("report.rmd", tempReport, overwrite=TRUE)
        # measurements for plot
        plotHeight = session$clientData$output_topNPlot_width * (1 + round((3/7)*log(length(input$selectVarsTop))))
        plotWidth = session$clientData$output_topNPlot_width
        # parameters for the report
        params <- list(metric = isolate(input$checkReportParameters),
                       variables = isolate(input$selectVarsTop),
                       identity = isolate(input$selectIdentityTop),
                       amount = isolate(input$selectTopN),
                       orient = isolate(input$radioTopOrBottom),
                       cat = isolate(input$markHDICategory),
                       plotWidth = isolate(plotWidth),
                       plotHeight = isolate(plotHeight),
                       dataset = isolate(dem)
                      )
        # render report
         rmarkdown::render(tempReport, 
                           output_format = switch(input$outputFileFormat,PDF = 'pdf_document', HTML = 'html_document', Word = 'word_document'),
                           output_file = file,
                           params = params,
                           envir = new.env(parent = globalenv())
        )
      }
    )

    # Interactive plot for aggregate per HDI
    output$plotlyInteractivePlot <- plotly::renderPlotly({
      plotlyDataset <- aggregate(formula(str_interp("${input$plotlyVarSelect} ~ hdi_cat")), data=dem, FUN=switch(input$plotlyFUNSelect, mean=mean, median=median, min=min, max=max))
      plot_ly(
        x = plotlyDataset[,2],
        y = plotlyDataset[,1],
        type = "bar"
      )
    })
  }
)
