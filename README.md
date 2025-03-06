# Statistical Analysis Dashboard

A Shiny application for visualizing and analyzing statistical distributions.

## Features

- **Continuous Distributions**: 
  - Normal Distribution
  - T Distribution
  - Chi-Square Distribution
  - Exponential Distribution

- **Download Options**:
  - Download individual plots as PDF
  - Generate comprehensive reports
  - Track analysis history

## How to Use

1. Select "Statistical Analysis" from the sidebar
2. Choose the distribution type (Continuous or Discrete)
3. Select the specific distribution
4. Set parameters based on your data
5. Generate and analyze plots

## Requirements

The application requires the following R packages:
- shiny
- shinydashboard
- shinyjs
- ggplot2
- plotly
- shinyBS
- DT
- dplyr
- rmarkdown
- glue
- moments

## Installation

```r
# Install required packages
install.packages(c("shiny", "shinydashboard", "shinyjs", "ggplot2", "plotly", 
                  "shinyBS", "DT", "dplyr", "rmarkdown", "glue", "moments"))

# Run the application
shiny::runApp("path/to/app")
``` 