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

In this section scatter plots can be created between 2 variables in the dataset (randomized upon entry), colour those by HDI category, switch the variables' axis.

#### Core functionality

#### Options

### Aggregates per HDI

#### Core functionality

#### Options

### Top n countries

#### Core functionality

#### Options

