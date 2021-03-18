# Shiny application: Development dataset

## Introduction

Obtained from the World bank Databank specifically the World Development Indicators database This is the 'primary World Bank collection of development indicators' as stated on the database description. It has lots of economic, education, energy use, and population specific metrics.

The dataset used (as it was used) can be found in the [file tree of my Statistical Learning project](https://github.com/dreth/UC3MStatisticalLearning/tree/main/data). The file path is as follows: /data/without_tags/data.csv.

## App guide: Tabs

- Introduction
- Histograms/Boxpots
- Correlation plots
- Aggregates per HDI
- Top n countries

## Tab functionality guide

### Introduction

Basic introduction of the dataset, contains each variable in the dataset described in detail.

### Histograms/Boxplots

This section is based upon a quite full features function I created for my statistical learning final project. It can create a boxplot and histogram/density plot per variable and per variable filtered by HDI category.

#### Core functionality

Select a variable in the dataset (out of those numeric variables) to generate 4 plots in the same grid:

- General boxplot of the variable
- Boxplots of the variable per HDI category
- Histogram/Density or Histogram + Density plot of the variable
- Histogram/Density or Histogram + Density plot of the variable per HDI category (ideally a single option is chosen: histogram, or density plot)

#### Options

Plot settings tab:

- Select variable
- Select between plotting (TRUE) and not plotting a histogram for the variable by itself (FALSE), (third plot in the grid)
- Select between plotting (TRUE) and not plotting a density curve for the variable by itself (FALSE), (third plot in the grid)
- Select between plotting (TRUE) and not plotting a histogram for the variable separated by HDI category (FALSE), (fourth plot in the grid)
- Use facet wrap (TRUE) or not (FALSE), this will separate the fourth plot in 4 different plots (one per HDI category) or plot them all together in the same figure
- Number of bins for the histogram of the variable standalone
- Number of bins for the histogram of the variable grouped by HDI category
- Height of the figure (in **pixels**), this controls the visualization on the page

Save plot tab:

- Width and height for this section are shown in **inches**.
- Save plot button (opens a save file dialog to download the plot as an image)

### Correlation plots

#### Core functionality

In this section we have the following functionality:

- Scatter plots can be created between 2 variables in the dataset (randomized upon entry), we can colour those by HDI category and switch the variables' axis
- Generate a *PerformanceAnalytics* correlation chart
- Generate a correlation matrix heatmap (only utilizes the maximum correlation coefficient of the 3 available in **cor**: *kendall*, *spearman* and *pearson*) 
- Shows a correlation table with the previously mentioned 3 correlation coefficients computed for the 2 variables selected.

#### Options

- Select plot type (selects between scatter plot, correlation matrix heatmap and *PerformanceAnalytics* correlation chart)
- Select variable 1 and 2 (for scatter plot and correlation table)
- Switch variables (to switch between variable 1 and 2's axes in the plot)
- Color by HDI (allows colouring the points in the plot by HDI category)

### Aggregates per HDI

In this section a Plotly plot is generated of a simple bar plot.

#### Core functionality

- Generate barplot of aggregate values for the specified variable 
- Select and determine which aggregation function is used for the variable sleected

#### Options

- Select variable to perform the aggregation and generate the barplot
- Select aggregation function to employ

### Top n countries 

This tab plots or generates a report of top/bottom N countries for the variable(s) selected.

#### Core functionality

- Generate a plot or multiple plot grid of top/bottom N countries for the specified variable
- Plots can be generated with either country names or ISO-3166 alpha-3 country codes
- Plots can be coloured by their respective HDI category
- Generate a report with simple statistics generated per variable selected. This report can be generated in either PDF, HTML or Word format

#### Options

Information tab:

Basic information on the section's functionality.

Plot parameters:

- Select variable(s) to plot top/bottom N countries (respective to that variable) in the figure
- Select ISO-3166 alpha-3 country code or country name to assign to the x-axis of the barplot
- Select how many countries to consider (default is 10, which in turn means 10 countries will appear in the plot, this can be modified up to the total amount of countries)
- Select to plot top or bottom N countries
- Select whether to colour the bars by HDI category or not

Report parameters:

- Selection of metrics for the statistical summary table (generates a table for all variable selected for the plots and the metrics selected in this checkbox field)
- Selection of file format to generate the report as