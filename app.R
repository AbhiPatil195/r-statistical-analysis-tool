library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plotly)
library(shinyBS)
library(DT)
library(dplyr)
library(rmarkdown)
library(glue)
library(moments)  # Added for skewness calculation

# Mock Data
set.seed(123)  # For reproducibility
mock_data <- data.frame(
  Sample_size = c(50, 60, 55, 70),
  Concentration = c(50, 45, 40, 35),  
  NormalData = rnorm(100, mean = 50, sd = 10),
  ChiSquareData = rchisq(100, df = 2),  # Chi-Square data with 2 degrees of freedom
  ExponentialData = rexp(100, rate = 1)  # Exponential data with rate λ = 1
)

ui <- dashboardPage(
  dashboardHeader(title = "STAT Analysis"),
  dashboardSidebar(
    actionButton("alldata", "All Report", icon = icon("file-alt"), class = "black-btn"),
    actionButton("statanalysis", "Statistical Analysis", icon = icon("chart-line"), class = "black-btn")
  ),
  dashboardBody(
    useShinyjs(),
    
    bsCollapse(
      id = "allresults", open = "Panel 5",
      bsCollapsePanel("All Report", style = "info",
                      h4(strong("Download All Results")),
                      downloadButton("download_results", "Download All Results"),
                      br(),  
                      br(),
                      h4(strong("Analysis History")),
                      DTOutput("history_table")  
      )
    ),
    bsCollapse(
      id = "datastatanalysis", open = "Panel 4",
      bsCollapsePanel("Statistical Analysis", style = "info",
                      actionButton("stat", "Statistics", style = "background-color: #4CAF50; color: white; border-radius: 5px;"),
                      conditionalPanel(
                        condition = "input.stat > 0",
                        uiOutput("dynamic_test_selection"),
                        actionButton("perform_test", "Proceed to Analysis")
                      )
      )
    )
  )
)

server <- function(input, output, session) {
  results <- reactiveValues(
    nrmplot = NULL,
    tplot = NULL,
    chi_square_plot = NULL,
    exp_plot = NULL,
    skewness_value = NULL,
    skewness_interpretation = NULL,
    history = data.frame(
      TestName = character(), 
      Date = character(), 
      View = character(), 
      Delete = character(), 
      stringsAsFactors = FALSE
    )
  )
  
  observeEvent(input$stat, {
    shinyjs::show("datastatanalysis")
    output$dynamic_test_selection <- renderUI({
      req(input$stat)
      selectInput(
        "test_type",
        "Select Test to Perform:",
        choices = c("Select Test", "Continuous Distribution", "Discrete Distribution"),
        selected = NULL,
        multiple = FALSE
      )
    })
    
    observeEvent(input$perform_test, {
      test_selected <- input$test_type
      if (test_selected == "Continuous Distribution") {
        showModal(modalDialog(
          title = "Continuous Distribution",
          fluidRow(
            column(4,
                   wellPanel(
                     selectInput(
                       "continuous_test",
                       "Select Test to Perform:",
                       choices = c("Select", "Normal Distribution", "T Distribution", "Chi-Square Distribution", "Exponential Distribution"),
                       selected = NULL,
                       multiple = FALSE
                     ),
                     uiOutput("sidebar_ui_continuous"),
                     actionButton("plotBtnContinuous", "Generate Plot", style = "background-color: #4CAF50; color: white; border-radius: 5px;")
                   )
            ),
            column(8, 
                   tabPanel("Plot",
                            conditionalPanel(
                              condition = "input.continuous_test == 'Normal Distribution'",
                              plotlyOutput("normPlot"),
                              textOutput("normPlotInfo"), 
                              downloadButton("download_norm", "Download Normal Dist Plot as PDF")
                            ),
                            conditionalPanel(
                              condition = "input.continuous_test == 'T Distribution'",
                              plotlyOutput("tplot"),
                              textOutput("tPlotInfo"),
                              downloadButton("download_t", "Download T Dist Plot as PDF")
                            ),
                            conditionalPanel(
                              condition = "input.continuous_test == 'Chi-Square Distribution'",
                              plotlyOutput("chiSquarePlot"),
                              textOutput("chiSquarePlotInfo"),
                              downloadButton("download_chi_square", "Download Chi-Square Plot as PDF")
                            ),
                            conditionalPanel(
                              condition = "input.continuous_test == 'Exponential Distribution'",
                              plotlyOutput("expPlot"),
                              textOutput("expPlotInfo"),
                              downloadButton("download_exp", "Download Exponential Plot as PDF")
                            )
                   )
            )
          ),
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          style = "width: 100%; height: 100%; max-width: none; max-height: none; margin: 0; padding: 0; overflow: auto;"
        ))
      } else if (test_selected == "Discrete Distribution") {
        showModal(modalDialog(
          title = "Discrete Distribution",
          fluidRow(
            column(4,
                   wellPanel(
                     selectInput(
                       "discrete_test",
                       "Select Test to Perform:",
                       choices = c("Select", "Binomial", "Poisson"),
                       selected = NULL,
                       multiple = FALSE
                     ),
                     uiOutput("sidebar_ui_discrete"),
                     actionButton("plotBtnDiscrete", "Generate Plot", style = "background-color: #4CAF50; color: white; border-radius: 5px;")
                   )
            ),
            column(8, 
                   tabsetPanel(
                     tabPanel("Plot", plotlyOutput("discretePlot"))
                   )
            )
          ),
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          style = "width: 100%; height: 100%; max-width: none; max-height: none; margin: 0; padding: 0; overflow: auto;"
        ))
      }
    })
    
    output$sidebar_ui_continuous <- renderUI({
      req(input$continuous_test)
      if (input$continuous_test == "Normal Distribution") {
        tagList(
          selectInput("column", "Select a Column:", choices = names(mock_data)),
          numericInput("mu", "Mean (µ):", value = 50, step = 0.1, width = '100%'),
          bsTooltip("mu", "Mean of the distribution", "right"),
          numericInput("sigma", "Standard Deviation (σ):", value = 10, step = 0.1, width = '100%'),
          bsTooltip("sigma", "Standard deviation of the distribution", "right")
        )
      } else if (input$continuous_test == "T Distribution") {
        tagList(
          selectInput("column", "Select a Column:", choices = names(mock_data)),
          sliderInput("df", "Degrees of Freedom (df):", min = 1, max = 30, value = 5, step = 1, width = '100%')
        )
      } else if (input$continuous_test == "Chi-Square Distribution") {
        tagList(
          selectInput("column", "Select a Column:", choices = names(mock_data)),
          numericInput("chi_df", "Degrees of Freedom (df):", value = 2, min = 1, step = 1, width = '100%')
        )
      } else if (input$continuous_test == "Exponential Distribution") {
        tagList(
          selectInput("column", "Select a Column:", choices = names(mock_data)),
          numericInput("lambda", "Rate (λ):", value = 1, step = 0.1, width = '100%')
        )
      }
    })
    
    observeEvent(input$plotBtnContinuous, {
      if (input$continuous_test == "Normal Distribution") {
        output$normPlot <- renderPlotly({
          req(input$column, input$mu, input$sigma)
          col_data <- mock_data[[input$column]]
          x_vals <- seq(min(col_data, na.rm = TRUE) - 20, 
                        max(col_data, na.rm = TRUE) + 20, length.out = 100)
          y_vals <- dnorm(x_vals, mean = input$mu, sd = input$sigma)
          
          nrm <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
            geom_line(color = "blue", size = 1) +
            geom_vline(xintercept = input$mu, color = "red", linetype = "dashed") +
            labs(title = paste("Normal Distribution for", input$column),
                 x = "Value of the Variable",
                 y = "Probability Density") +
            theme_minimal()
          
          results$nrmplot <- nrm
          
          ggplotly(nrm)
        })
        
        output$normPlotInfo <- renderText({
          req(input$column)  # Ensure the column is selected
          col_data <- mock_data[[input$column]]
          results$skewness_value <- skewness(col_data)  # Calculate skewness dynamically
          
          skewness_interpretation <- if (results$skewness_value > 0) {
            "The skewness value is positive, indicating a right-skewed distribution (longer tail on the right)."
          } else if (results$skewness_value < 0) {
            "The skewness value is negative, indicating a left-skewed distribution (longer tail on the left)."
          } else {
            "The skewness value is zero, indicating a symmetric distribution."
          }
          
          results$skewness_interpretation <- skewness_interpretation
          
          paste("This plot represents the normal distribution with mean =", input$mu, 
                "and standard deviation =", input$sigma, ".",
                "The normal distribution is a continuous probability distribution characterized by its bell-shaped curve.",
                "It is defined by two parameters: the mean (average) and the standard deviation (spread).",
                "Skewness is a measure of the asymmetry of a distribution",
                "Skewness: ", round(results$skewness_value, 2), " ",
                skewness_interpretation)
        })
        
        new_entry <- data.frame(
          TestName = "Normal Distribution",
          Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View", onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete", onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          stringsAsFactors = FALSE
        )
        results$history <- rbind(results$history, new_entry)
        
      } else if (input$continuous_test == "T Distribution") {
        output$tplot <- renderPlotly({
          req(input$column, input$df)
          
          col_data <- mock_data[[input$column]]
          x_vals <- seq(min(col_data, na.rm = TRUE) - 10, 
                        max(col_data, na.rm = TRUE) + 10, length.out = 100)
          y_vals <- dt(x_vals, df = input$df)
          
          tp <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
            geom_line(color = "blue", size = 1) +
            geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
            labs(title = paste("T-Distribution (df =", input$df, ")"),
                 x = "Value",
                 y = "Density") +
            theme_minimal()
          
          results$tplot <- tp
          
          ggplotly(tp)
        })
        
        output$tPlotInfo <- renderText({
          req(input$df)
          paste("This plot represents the T-distribution with degrees of freedom =", input$df, ".",
                "The T-distribution is used in statistics to estimate population parameters when the sample size is small.",
                "It is similar to the normal distribution but has heavier tails, which provides a better estimate for small samples.",
                "As the degrees of freedom increase, the T-distribution approaches a normal distribution.")
        })
        
        new_entry <- data.frame(
          TestName = "T Distribution",
          Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View", onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete", onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          stringsAsFactors = FALSE
        )
        results$history <- rbind(results$history, new_entry)
        
      } else if (input$continuous_test == "Chi-Square Distribution") {
        output$chiSquarePlot <- renderPlotly({
          req(input$chi_df, input$column)
          
          col_data <- mock_data[[input$column]]
          x_vals <- seq(0, max(col_data) + 10, length.out = 100)
          y_vals <- dchisq(x_vals, df = input$chi_df)
          
          chi_sq <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
            geom_line(color = "blue", size = 1) +
            labs(title = paste("Chi-Square Distribution (df =", input$chi_df, ")"),
                 x = "Value",
                 y = "Density") +
            theme_minimal()
          
          results$chi_square_plot <- chi_sq
          
          ggplotly(chi_sq)
        })
        
        output$chiSquarePlotInfo <- renderText({
          req(input$chi_df)
          paste("This plot represents the Chi-Square distribution with degrees of freedom =", input$chi_df, ".",
                "The Chi-Square distribution is commonly used in hypothesis testing and in constructing confidence intervals.",
                "It is a right-skewed distribution, and as the degrees of freedom increase, it becomes more symmetric.")
        })
        
        new_entry <- data.frame(
          TestName = "Chi-Square Distribution",
          Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View", onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete", onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          stringsAsFactors = FALSE
        )
        results$history <- rbind(results$history, new_entry)
        
      } else if (input$continuous_test == "Exponential Distribution") {
        output$expPlot <- renderPlotly({
          req(input$lambda, input$column)
          
          col_data <- mock_data[[input$column]]
          x_vals <- seq(0, 10, length.out = 100)
          y_vals <- dexp(x_vals, rate = input$lambda)
          
          exp_dist <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
            geom_line(color = "blue", size = 1) +
            labs(title = paste("Exponential Distribution (λ =", input$lambda, ")"),
                 x = "Value",
                 y = "Density") +
            theme_minimal()
          
          results$exp_plot <- exp_dist
          
          ggplotly(exp_dist)
        })
        
        output$expPlotInfo <- renderText({
          req(input$lambda)
          paste("This plot represents the Exponential distribution with rate =", input$lambda, ".",
                "The Exponential distribution is often used to model the time until an event occurs, such as failure or arrival.",
                "It is a continuous distribution with a constant hazard rate, meaning the event rate is constant over time.")
        })
        
        new_entry <- data.frame(
          TestName = "Exponential Distribution",
          Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View", onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete", onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          stringsAsFactors = FALSE
        )
        results$history <- rbind(results$history, new_entry)
      }
    })
    
    output$download_norm <- downloadHandler(
      filename = function() {
        paste(input$continuous_test, "plot", format(Sys.Date(), "%Y-%m-%d"), "-", format(Sys.time(), "%H-%M-%S"), ".pdf", sep = "")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)  # Set PDF dimensions
        req(input$column, input$mu, input$sigma)  # Ensure inputs are available
        col_data <- mock_data[[input$column]]
        x_vals <- seq(min(col_data, na.rm = TRUE) - 20, 
                      max(col_data, na.rm = TRUE) + 20, length.out = 100)
        y_vals <- dnorm(x_vals, mean = input$mu, sd = input$sigma)
        
        nrm <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
          geom_line(color = "blue", size = 1) +
          geom_vline(xintercept = input$mu, color = "red", linetype = "dashed") +
          labs(title = paste("Normal Distribution for", input$column),
               x = "Value of the Variable",
               y = "Probability Density") +
          theme_minimal()
        
        print(nrm)  # Print the plot to the PDF
        
        dev.off()
      }
    )
    
    output$download_t <- downloadHandler(
      filename = function() {
        paste(input$continuous_test, "T-distribution plot", format(Sys.Date(), "%Y-%m-%d"), "-", format(Sys.time(), "%H-%M-%S"), ".pdf", sep = "")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)  # Set PDF dimensions
        req(input$column, input$df)  # Ensure inputs are available
        
        col_data <- mock_data[[input$column]]
        x_vals <- seq(min(col_data, na.rm = TRUE) - 10, 
                      max(col_data, na.rm = TRUE) + 10, length.out = 100)
        y_vals <- dt(x_vals, df = input$df)
        
        tp <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
          geom_line(color = "blue", size = 1) +
          geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
          labs(title = paste("T-Distribution (df =", input$df, ")"),
               x = "Value",
               y = "Density") +
          theme_minimal()
        
        print(tp)  # Print the plot to the PDF
        
        dev.off()
      }
    )
    
    output$download_chi_square <- downloadHandler(
      filename = function() {
        paste("Chi-Square Distribution plot", format(Sys.Date(), "%Y-%m-%d"), "-", format(Sys.time(), "%H-%M-%S"), ".pdf", sep = "")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)  # Set PDF dimensions
        req(input$chi_df, input$column)  # Ensure inputs are available
        
        col_data <- mock_data[[input$column]]
        x_vals <- seq(0, max(col_data) + 10, length.out = 100)
        y_vals <- dchisq(x_vals, df = input$chi_df)
        
        chi_sq <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
          geom_line(color = "blue", size = 1) +
          labs(title = paste("Chi-Square Distribution (df =", input$chi_df, ")"),
               x = "Value",
               y = "Density") +
          theme_minimal()
        
        print(chi_sq)  # Print the plot to the PDF
        
        dev.off()
      }
    )
    
    output$download_exp <- downloadHandler(
      filename = function() {
        paste("Exponential Distribution plot", format(Sys.Date(), "%Y-%m-%d"), "-", format(Sys.time(), "%H-%M-%S"), ".pdf", sep = "")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)  # Set PDF dimensions
        req(input$lambda, input$column)  # Ensure inputs are available
        
        col_data <- mock_data[[input$column]]
        x_vals <- seq(0, 10, length.out = 100)
        y_vals <- dexp(x_vals, rate = input$lambda)
        
        exp_dist <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
          geom_line(color = "blue", size = 1) +
          labs(title = paste("Exponential Distribution (λ =", input$lambda, ")"),
               x = "Value",
               y = "Density") +
          theme_minimal()
        
        print(exp_dist)  # Print the plot to the PDF
        
        dev.off()
      }
    )
    
    output$sidebar_ui_discrete <- renderUI({
      req(input$discrete_test)
      # Discrete tests are left empty for now
    })
    
    observeEvent(input$plotBtnDiscrete, {
      output$discretePlot <- renderPlotly({
        req(input$discrete_test)
        # Discrete plot logic is left empty for now
      })
    })
  }) # stat  
  
  # Render history table
  output$history_table <- renderDT({
    datatable(results$history, escape = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$view, {
    selected_row <- as.numeric(input$view)
    if (selected_row <= nrow(results$history)) {
      test_name <- results$history[selected_row, "TestName"]
      plot_data <- NULL
      if (test_name == "Normal Distribution") {
        plot_data <- results$nrmplot
      } else if (test_name == "T Distribution") {
        plot_data <- results$tplot
      } else if (test_name == "Chi-Square Distribution") {
        plot_data <- results$chi_square_plot
      } else if (test_name == "Exponential Distribution") {
        plot_data <- results$exp_plot
      }
      output$downloadPDF <- downloadHandler(
        filename = function() {
          filename <- paste0(gsub(" ", "", test_name), "_results", format(Sys.Date(), "%Y-%m-%d"), "-", format(Sys.time(), "%H-%M-%S"), ".pdf")
        },
        content = function(file) {
          rmd_content <- ""
          # Generate R Markdown content based on test type
          if (test_name == "Normal Distribution") {
            rmd_content <- glue('
            *Interpretation:*\n\n
            *This plot represents the normal distribution with mean: {input$mu}, **and standard deviation*: {input$sigma}\n\n
            *The normal distribution is a continuous probability distribution characterized by its bell-shaped curve.*\n\n
            *It is defined by two parameters: the mean (average) and the standard deviation (spread).*\n\n
            *Skewness is a measure of the asymmetry of a distribution.*\n\n
            *Skewness:* {round(results$skewness_value, 2)}\n\n
            *Interpretation:* {results$skewness_interpretation}\n\n
            *Normal Distribution Plot*\n\n
            {{r, echo=FALSE, fig.cap="Normal Distribution Plot"}}
            print(plot_data)
            
            ')
          } else if (test_name == "T Distribution") {
            rmd_content <- glue('
            *Interpretation:*\n\n
            *This plot represents the T-distribution with degrees of freedom*: {input$df}\n\n
            *The T-distribution is used in statistics to estimate population parameters when the sample size is small. It is similar to the normal distribution but has heavier tails, which provides a better estimate for small samples.*\n\n
            *T Distribution Plot*\n\n
            {{r, echo=FALSE, fig.cap="T Distribution Plot"}}
            print(plot_data)
            
            ')
          } else if (test_name == "Chi-Square Distribution") {
            rmd_content <- glue('
            *Interpretation:*\n\n
            *This plot represents the Chi-Square distribution with degrees of freedom*: {input$chi_df}\n\n
            *The Chi-Square distribution is commonly used in hypothesis testing and in constructing confidence intervals.*\n\n
            *Chi-Square Distribution Plot*\n\n
            {{r, echo=FALSE, fig.cap="Chi-Square Distribution Plot"}}
            print(plot_data)
            
            ')
          } else if (test_name == "Exponential Distribution") {
            rmd_content <- glue('
            *Interpretation:*\n\n
            *This plot represents the Exponential distribution with rate*: {input$lambda}\n\n
            *The Exponential distribution is often used to model the time until an event occurs, such as failure or arrival.*\n\n
            *Exponential Distribution Plot*\n\n
            {{r, echo=FALSE, fig.cap="Exponential Distribution Plot"}}
            print(plot_data)
            
            ')
          }
          
          tmp_rmd <- tempfile(fileext = ".Rmd")
          writeLines(c(
            "---",
            "title: \"Statistical Analysis Results\"",
            "output: pdf_document",
            "---",
            "",
            rmd_content
          ), tmp_rmd)
          rmarkdown::render(tmp_rmd, output_file = file, quiet = TRUE)
          unlink(tmp_rmd)
        }
      )
      
      output$view_plot <- renderPlot({
        if (!is.null(plot_data)) {
          print(plot_data)
        }
      })
      
      showModal(modalDialog(
        title = glue("Test Result for {test_name}"),
        renderUI({
          if (!is.null(plot_data)) {
            tagList(
              plotOutput("view_plot")
            )
          } else {
            "No results available."
          }
        }),
        easyClose = TRUE,
        footer = tagList(
          div(style = "display: flex; justify-content: space-between; width: 100%;",
              downloadButton("downloadPDF", "Download PDF"),
              modalButton("Close")
          )
        )
      ))
      
      # Handle delete action
      observeEvent(input$delete, {
        selected_row <- as.numeric(input$delete)
        if (selected_row <= nrow(results$history)) {
          results$history <- results$history[-selected_row, ]
        }
      })
    }
  })
  
  # Download combined results functionality
  output$download_results <- downloadHandler(
    filename = function() {
      paste("combined_statistical_report_", format(Sys.Date(), "%Y-%m-%d"), "-", format(Sys.time(), "%H-%M-%S"), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      # Start writing the R Markdown content
      report_lines <- c(
        "---",
        "title: 'Combined Statistical Report'",
        "output: pdf_document",
        "header-includes:",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{float}",
        "params:",
        "  nrm: NULL",
        "  nrmv1: NULL",
        "  nrmv2: NULL",
        "  nrmskew : NULL",
        "  skew_interpretation: NULL",  # Add this line
        "  tdist: NULL",
        "  tvar : NULL",
        "  chisq: NULL",
        "  chi_df: NULL",
        "  exp: NULL",
        "  lambda: NULL",
        "---",
        "",
        "# Results",
        ""
      )
      
      # Loop through the history and add results to the report
      for (i in 1:nrow(results$history)) {
        test_name <- results$history$TestName[i]
        
        if (test_name == "Normal Distribution" && !is.null(results$nrmplot)) {
          report_lines <- c(report_lines,
                            "### Normal Distribution Plot",
                            "",
                            "#### Plot Information",
                            "",
                            "*This plot represents the normal distribution with mean: r params$nrmv1, **and standard deviation*: r params$nrmv2",
                            "",
                            "It is defined by two parameters: the mean (average) and the standard deviation (spread).",
                            "",
                            "Skewness is a measure of the asymmetry of a distribution.",
                            "",
                            " *Skewness*: r round(params$nrmskew, 2)",
                            "",
                            " r params$skew_interpretation",  # Add this line
                            "",
                            "{r, echo=FALSE, fig.height=6, fig.width=8, fig.pos='H'}",
                            "print(params$nrm)",
                            "",
                            ""                
          )
        }
        
        if (test_name == "T Distribution" && !is.null(results$tplot)) {
          report_lines <- c(report_lines,
                            "### T Distribution Plot",
                            "",
                            "#### Plot Information",
                            "",
                            "This plot represents the T-distribution with degrees of freedom :r params$tvar", 
                            "",
                            "The T-distribution is used in statistics to estimate population parameters when the sample size is small.",
                            "",
                            "It is similar to the normal distribution but has heavier tails, which provides a better estimate for small samples.",
                            "",
                            "{r, echo=FALSE, fig.height=6, fig.width=8, fig.pos='H'}",
                            "print(params$tdist)",
                            "",
                            ""
          )
        }
        
        if (test_name == "Chi-Square Distribution" && !is.null(results$chi_square_plot)) {
          report_lines <- c(report_lines,
                            "### Chi-Square Distribution Plot",
                            "",
                            "#### Plot Information",
                            "",
                            "This plot represents the Chi-Square distribution with degrees of freedom :r params$chi_df", 
                            "",
                            "The Chi-Square distribution is commonly used in hypothesis testing and in constructing confidence intervals.",
                            "",
                            "{r, echo=FALSE, fig.height=6, fig.width=8, fig.pos='H'}",
                            "print(params$chisq)",
                            "",
                            ""
          )
        }
        
        if (test_name == "Exponential Distribution" && !is.null(results$exp_plot)) {
          report_lines <- c(report_lines,
                            "### Exponential Distribution Plot",
                            "",
                            "#### Plot Information",
                            "",
                            "This plot represents the Exponential distribution with rate :r params$lambda", 
                            "",
                            "The Exponential distribution is often used to model the time until an event occurs, such as failure or arrival.",
                            "",
                            "{r, echo=FALSE, fig.height=6, fig.width=8, fig.pos='H'}",
                            "print(params$exp)",
                            "",
                            ""
          )
        }
      }
      
      writeLines(report_lines, con = tempReport)
      
      # Render the R Markdown report to PDF
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = list(
                          nrm = results$nrmplot,  
                          nrmv1 = input$mu,  
                          nrmv2 = input$sigma,
                          nrmskew = results$skewness_value,
                          interpretation = results$skewness_interpretation,  
                          tdist = results$tplot,
                          tvar =  input$df, 
                          chisq = results$chi_square_plot,
                          chi_df = input$chi_df,
                          exp = results$exp_plot,
                          lambda = input$lambda
                        ),
                        quiet = TRUE)
    }
  )
}

shinyApp(ui, server) 