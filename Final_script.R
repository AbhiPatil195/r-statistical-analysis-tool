#without plot for t test, only plot for pca
library(shiny)
library(shinydashboard)
library(shinyTree)
library(shinyjs)  # For controlling UI visibility
library(jsonlite)  # For converting data to JSON
library(shinyBS)  # For collapsible panels
library(DT)  # For displaying tables
library(plotly)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(tinytex)
library(rmarkdown)
library(knitr)      
library(kableExtra)
library(reshape2)
library(glue)
library(grid)
library(gridExtra)
library(png)
library(processx)



ui <- dashboardPage(
  dashboardHeader(title = "STAT Analysis"),
  
  dashboardSidebar(
    actionButton("alldata", "All Report", icon = icon("file-alt"), class = "black-btn", style = "width: 200px;"),
    actionButton("ingestData", "Data Ingestion", icon = icon("cloud-upload-alt"), class = "black-btn", style = "width: 200px;"),
    actionButton("manipulation", "Data Manipulation", icon = icon("cogs"), class = "black-btn", style = "width: 200px;"),
    actionButton("preprocessing", "Data Pre-processing", icon = icon("database"), class = "black-btn", style = "width: 200px;"),
    actionButton("statanalysis", "Statistical Analysis", icon = icon("chart-line"), class = "black-btn", style = "width: 200px;")
  ),
  
  dashboardBody(
    useShinyjs(),  # Initialize shinyjs
    
    # All results
    hidden(
      bsCollapse(id = "allresults", open = "Panel 5",
                 bsCollapsePanel("All Report", style = "info",
                                 
                                 h4(strong("Download All Results")),  # Title for the download button
                                 downloadButton("download_results", "Download All Results"),
                                 br(),  
                                 br(),
                                 h4(strong("Analysis History")), # Title for the history table
                                 DTOutput("history_table")  
                 )
      )
    ),
    
    # Data Ingestion Panel
    hidden(
      bsCollapse(id = "dataingesion", open = "Panel 1",
                 bsCollapsePanel("Project Structure", style = "info",
                                 fileInput("fileUpload", "Upload Excel File:", accept = c(".xlsx")),
                                 shinyTree("fileTree", checkbox = FALSE),  # Tree view with sheets and files
                                 hr(),
                                 verbatimTextOutput("selectedFileName"),  # Display uploaded file name
                                 hr(),
                                 selectInput("fileSelect", "Select File:", choices = NULL),  # File selection
                                 hr(),
                                 uiOutput("sheetNamesOutput"),  # Checkbox for sheet selection
                                 hr(),
                                 h4("Selected Sheets:"),
                                 verbatimTextOutput("selectedSheetNames"),  # Display selected sheet names
                                 hr(),
                                 actionButton("saveData", "Save Data"),  # Button to save data
                                 verbatimTextOutput("saveStatus")  # Display save status
                 )
      )
    ),
    
    # Data Manipulation Panel
    hidden(
      bsCollapse(id = "datamanipulation", open = "Panel 2",
                 bsCollapsePanel("Data Manipulation", style = "info",
                                 h3("Data Manipulation"),
                                 hr(),
                                 uiOutput("dynamic_table_selection"),  # Dynamic table selection
                                 uiOutput("dynamic_operation_selection"),  # Dynamic operation selection
                                 uiOutput("column_ui"),  # Dynamic column selection
                                 actionButton("process", "Perform Operation"),  # Perform Operation
                                 hr(),
                                 conditionalPanel(
                                   condition = "input.data_selection.length > 0",
                                   uiOutput("tables_display"),  # Display selected tables here
                                   hr(),
                                   h4("Result of Operation"),
                                   DTOutput("result_table")  # Output for result table
                                 )
                 )
      )
    ),
    
    hidden(
      bsCollapse(id = "datapreprocessing", open = "Panel 3",
                 bsCollapsePanel("Data Pre-processing", style = "info",
                                 uiOutput("column_selection_ui"),  # Column selection dropdown for result data
                                 hr(),
                                 conditionalPanel(
                                   condition = "input.column1 != null",  # Show when 'column1' is selected
                                   uiOutput("preprocessing_options_ui"), # Preprocessing options dropdown
                                   uiOutput("process_button_ui"),
                                   DTOutput("processed_table")
                                 )
                 )
      )
    ),
    
    # Statistical Analysis Panel
    hidden(
      bsCollapse(id = "datastatanalysis", open = "Panel 4",
                 bsCollapsePanel("Statistical Analysis", style = "info",
                                 actionButton("stat", "Statistics", style = "background-color: #4CAF50; color: white; border-radius: 5px;"),
                                 conditionalPanel(
                                   condition = "input.stat > 0",  # Display UI only after clicking "Statistics" button
                                   uiOutput("dynamic_test_selection"),
                                   
                                   # Always show the "Proceed to Analysis" button after clicking statistics
                                   actionButton("perform_test", "Proceed to Analysis"),
                                   
                                   DTOutput("anovatable"),
                                   DTOutput("pcatable")
                                 ),
                                 actionButton("plot", "Visualization", style = "background-color: #4CAF50; color: white; border-radius: 5px;"),
                                 
                                 conditionalPanel(
                                   condition = "input.plot > 0",
                                   uiOutput("dynamic_plot_selection"),
                                   uiOutput("column_ui_selection1"),
                                   uiOutput("ucl_slider"),
                                   uiOutput("lcl_slider"),
                                   actionButton("generate_chart", "Quality Control Chart"),
                                   downloadButton("download_chart", "Download"),
                                   plotlyOutput("control_chart")
                                 )
                 )
      )
    ),
  ) ## dashboardBody
)   # dashboardPage

server <- function(input, output, session) {
  
  results <- reactiveValues(
    t_res_table1 = NULL,
    t_res_table2 = NULL,
    t_test_plot_static_one = NULL,
    t_test_plot_static_two  = NULL,
    t_test_interpretation_html = NULL,
    aov_res_table1  = NULL,
    aov_res_table2 = NULL,
    manova_res_table1 = NULL,
    manova_res_table2 = NULL,
    rep_anova = NULL ,
    pca_components = NULL,
    pca_explained_variance = NULL,
    pca_plot_static = NULL,
    pca_interpretation_text = NULL,
    history = data.frame(
      TestName = character(), 
      Date = character(), 
      View = character(), 
      Delete = character(), 
      stringsAsFactors = FALSE
    )
  )
  
  selected_sheets <- reactiveVal(character())
  saved_data <- reactiveVal(list())
  saved_json_data <- reactiveVal() 
  
  # Reactive value to store all uploaded files and their sheets
  uploaded_files <- reactiveVal(list())
  
  observeEvent(input$ingestData, {
    shinyjs::show("dataingesion")
  })
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)  # Ensure a file is uploaded
    
    # Read the uploaded file
    file_path <- input$fileUpload$datapath
    excel_data <- readxl::excel_sheets(file_path)  # Get sheet names
    
    # Update the UI with the uploaded file name
    output$selectedFileName <- renderText({
      paste("Uploaded File:", input$fileUpload$name)
    })
    
    # Add the new file and its sheets to the list of uploaded files
    current_files <- uploaded_files()
    current_files[[input$fileUpload$name]] <- excel_data
    uploaded_files(current_files)
    
    # Update the file tree structure
    tree_data <- setNames(lapply(names(current_files), function(f) {
      # Create children nodes (sheets) with table icons
      sheet_nodes <- setNames(lapply(current_files[[f]], function(sheet) {
        structure(list(), sticon = "fa fa-table")  # Sheets with table icon
      }), current_files[[f]])
      
      # Attach Excel file icon to the file
      structure(sheet_nodes, sticon = "fa fa-file-excel")
    }), names(current_files))
    
    
    # Render the tree structure
    output$fileTree <- renderTree({
      setNames(tree_data, names(current_files))
    })
    
    # Update the select input with all uploaded file names
    updateSelectInput(session, "fileSelect", choices = names(current_files))
    
    # Update the sheet names output
    output$sheetNamesOutput <- renderUI({
      if (!is.null(excel_data) && length(excel_data) > 0) {
        checkboxGroupInput("sheetSelect", "Select Sheets:", choices = excel_data, selected = NULL)
      } else {
        h4("No sheets found.")
      }
    })
    
    # Clear previous selections
    selected_sheets(character())
  })
  
  observeEvent(input$sheetSelect, {
    selected_sheets(input$sheetSelect)
    output$selectedSheetNames <- renderText({
      if (length(selected_sheets()) > 0) {
        paste("You have selected the following sheet(s):", paste(selected_sheets(), collapse = ", "))
      } else {
        "No sheets selected."
      }
    })
  })
  
  output$saveStatus <- renderText({
    if (length(saved_data()) > 0) {
      "The Sheets/Tabs chosen are saved successfully."
    } else {
      "Please select a file and sheets to save data."
    }
  })
  
  observeEvent(input$saveData, {
    selected_file <- input$fileSelect
    selected_sheet_names <- input$sheetSelect
    
    if (!is.null(selected_file) && length(selected_sheet_names) > 0) {
      # Read the data from the selected sheets
      file_path <- input$fileUpload$datapath
      data_to_save <- lapply(selected_sheet_names, function(sheet) {
        readxl::read_excel(file_path, sheet = sheet)
      })
      names(data_to_save) <- selected_sheet_names
      saved_data(data_to_save)
      
      # Convert the saved data to JSON
      json_data <- jsonlite::toJSON(saved_data(), pretty = TRUE, auto_unbox = TRUE)
      saved_json_data(json_data)
    } else {
      output$saveStatus <- renderText("Please select at least one sheet to save data.")
    }
  })
  
  # Observe the file selection and update the sheet names output accordingly
  observeEvent(input$fileSelect, {
    req(input$fileSelect)  # Ensure a file is selected
    selected_file <- input $fileSelect
    sheets_for_selected_file <- uploaded_files()[[selected_file]]
    
    output$sheetNamesOutput <- renderUI({
      if (!is.null(sheets_for_selected_file) && length(sheets_for_selected_file) > 0) {
        checkboxGroupInput("sheetSelect", "Select Sheets:", choices = sheets_for_selected_file, selected = NULL)
      } else {
        h4("No sheets found for the selected file.")
      }
    })
    
    # Clear previous selections when a new file is selected
    selected_sheets(character())
  })
  
  #_________________________________manipulation 
  stored_result <- reactiveVal()
  
  observeEvent(input$manipulation, {
    shinyjs::show("datamanipulation")
    
    # --- Render UI to select tables ---
    output$dynamic_table_selection <- renderUI({
      req(saved_json_data())
      json_data <- fromJSON(saved_json_data())
      
      if (length(json_data) == 0) {
        showNotification("No tables found in uploaded data.", type = "error")
        return(NULL)
      }
      
      selectInput("data_selection", "Select Table(s):", choices = names(json_data), multiple = TRUE)
    })
    
    # --- Render UI to select operation ---
    output$dynamic_operation_selection <- renderUI({
      req(input$data_selection)
      num_selected <- length(input$data_selection)
      
      if (num_selected == 2) {
        selectInput("operation", "Select Operation:",
                    choices = c("Match and Show Only Overlapping Rows (Inner Join)",
                                "Blend All Rows & Columns from Both Tables (Full Outer Join)",
                                "Show All Rows from Table 1, Matching with Table 2 (Left Join)",
                                "Show All Rows from Table 2, Matching with Table 1 (Right Join)",
                                "Show Rows Only Found in Table 1 (Left Anti Join)",
                                "Show Rows Only Found in Table 2 (Right Anti Join)"))
      } else if (num_selected > 2) {
        selectInput("operation", "Select Operation:",
                    choices = c("Combine All Rows from Selected Tables (Appending Rows)",
                                "Match & Include All Rows from Selected Tables (Full Outer Join)",
                                "Show Only Common Rows Across Selected Tables (Intersection)",
                                "Merge Unique Rows from Selected Tables (Union)",
                                "Show Only Rows from the First Table (Set Difference)"))
      } else {
        showNotification("Please select at least two tables for operation.", type = "error")
        return(NULL)
      }
    })
    
    # --- Render column selection UI ---
    output$column_ui <- renderUI({
      req(input$data_selection)
      json_data <- fromJSON(saved_json_data())
      selected_tables <- lapply(input$data_selection, function(name) json_data[[name]])
      
      if (length(selected_tables) < 2) {
        return(h4("Please select at least two tables."))
      }
      
      common_cols <- Reduce(intersect, lapply(selected_tables, colnames))
      
      
      selectInput("column", "Select Column(s) for Operation:", choices = common_cols, multiple = TRUE)
    })
    
    # --- Display selected tables ---
    output$tables_display <- renderUI({
      req(input$data_selection)
      tagList(lapply(input$data_selection, function(tbl_name) {
        bsCollapsePanel(
          title = tbl_name,
          dataTableOutput(outputId = paste0("table_", tbl_name)),
          style = "info"
        )
      }))
    })
    
    observe({
      req(input$data_selection)
      json_data <- fromJSON(saved_json_data())
      lapply(input$data_selection, function(tbl_name) {
        output[[paste0("table_", tbl_name)]] <- renderDataTable({
          datatable(json_data[[tbl_name]], options = list(scrollX = TRUE))
        })
      })
    })
    
    # --- Perform Operation ---
    result_data <- eventReactive(input$process, {
      req(input$operation, input$data_selection)
      json_data <- fromJSON(saved_json_data())
      selected_tables <- lapply(input$data_selection, function(name) json_data[[name]])
      result <- NULL
      
      if (length(selected_tables) < 2) {
        showNotification("Please select at least two tables for processing.", type = "error")
        return(NULL)
      }
      
      tryCatch({
        op <- input$operation
        cols <- input$column
        req(length(cols) > 0)
        
        # Convert all join keys to character for consistency
        selected_tables <- lapply(selected_tables, function(tbl) {
          for (col in cols) {
            tbl[[col]] <- as.character(tbl[[col]])
          }
          return(tbl)
        })
        
        # --- dplyr Operation Handling ---
        if (op == "Match and Show Only Overlapping Rows (Inner Join)") {
          result <- Reduce(function(x, y) inner_join(x, y, by = cols), selected_tables)
          
        } else if (op == "Blend All Rows & Columns from Both Tables (Full Outer Join)") {
          result <-  bind_rows(selected_tables)
          
        } else if (op == "Show All Rows from Table 1, Matching with Table 2 (Left Join)") {
          result <- Reduce(function(x, y) left_join(x, y, by = cols), selected_tables)
          
        } else if (op == "Show All Rows from Table 2, Matching with Table 1 (Right Join)") {
          result <- Reduce(function(x, y) right_join(x, y, by = cols), selected_tables)
          
        } else if (op == "Show Rows Only Found in Table 1 (Left Anti Join)") {
          result <- anti_join(selected_tables[[1]], selected_tables[[2]], by = cols)
          
        } else if (op == "Show Rows Only Found in Table 2 (Right Anti Join)") {
          result <- anti_join(selected_tables[[2]], selected_tables[[1]], by = cols)
          
        } else if (op == "Combine All Rows from Selected Tables (Appending Rows)") {
          result <- bind_rows(selected_tables)
          
        } else if (op == "Match & Include All Rows from Selected Tables (Full Outer Join)") {
          result <- Reduce(function(x, y) full_join(x, y, by = cols), selected_tables)
          
        } else if (op == "Show Only Common Rows Across Selected Tables (Intersection)") {
          result <- Reduce(function(x, y) inner_join(x, y, by = cols), selected_tables)
          
        } else if (op == "Merge Unique Rows from Selected Tables (Union)") {
          result <- bind_rows(selected_tables) %>% distinct()
          
        } else if (op == "Show Only Rows from the First Table (Set Difference)") {
          result <- Reduce(function(x, y) {
            anti_join(x, y, by = cols)
          }, selected_tables)
          
        }
        
        if (is.null(result) || nrow(result) == 0) {
          showNotification(
            HTML("<b>No result found.</b> Try selecting a different column or check for mismatched values."),
            type = "warning", duration = 5
          )
        }
        
        stored_result(result)
        return(result)
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
      })
    })
    # Render the result table after performing the operation
    output$result_table <- renderDT({
      req(result_data())
      datatable(
        result_data(),
        options = list(
          scrollX = TRUE,
          columnDefs = list(list(targets = "_all", className = "dt-center")),
          dom = 'lfrtip',  # Control the layout of the table elements
          selection = 'multiple',
          colVis = TRUE  # Enable column visibility toggle
        )
      )
    })
    
  })  
  #________________________________preprocessing_____________  
  observeEvent(input$preprocessing, {
    shinyjs::show("datapreprocessing")
    
    output$column_selection_ui <- renderUI({
      req(stored_result())
      colnames_result <- colnames(stored_result())
      if (length(colnames_result) > 0) {
        selectInput("column1", "Select Column(s) for Operation:", choices = colnames_result, multiple = TRUE)
      } else {
        h4("No columns available in the result data.")
      }
    })
    
    output$preprocessing_options_ui <- renderUI({
      req(input$column1)
      selectInput(
        "preprocessing_option",
        "Select Preprocessing Option:",
        choices = c("Handle missing values", "Remove duplicates", "Remove outliers"),
        selected = NULL
      )
    })
    
    output$process_button_ui <- renderUI({
      req(input$preprocessing_option)
      actionButton("process_data", "Process Data")
    })
    
    process_clicked <- reactiveVal(FALSE)
    
    observeEvent(input$process_data, {
      req(input$column1, input$preprocessing_option)
      selected_columns <- input$column1
      option <- input$preprocessing_option
      processed <- stored_result()
      
      if (option == "Handle missing values") {
        showModal(modalDialog(
          title = "Handle Missing Values",
          "Choose how to handle missing values for each selected column:",
          lapply(selected_columns, function(col) {
            tagList(
              h4(col),
              radioButtons(
                paste0("fill_option_", col),
                "Choose missing value strategy:",
                choices = {
                  if (is.numeric(processed[[col]])) {
                    c("Mean", "Median", "Specific Value", "Remove")
                  } else {
                    c("Mode", "Specific Value", "Remove")
                  }
                },
                selected = if (is.numeric(processed[[col]])) "Mean" else "Mode"
              ),
              conditionalPanel(
                condition = paste0("input.fill_option_", col, " == 'Specific Value'"),
                textInput(paste0("replace_", col), label = "Replace With:", value = "")
              ),
              
            )
          }),
          actionButton("confirm_replace_values", "OK"),
          easyClose = TRUE
        ))
        
        observeEvent(input$confirm_replace_values, {
          for (col in selected_columns) {
            # Step 2: Check that an option was selected.
            option <- input[[paste0("fill_option_", col)]]
            if (length(option) == 0) {
              showNotification(paste("No option selected for column:", col), type = "error")
              next  # Skip this column if no option is chosen.
            }
            
            # Retrieve any specific replacement value.
            replace_value <- input[[paste0("replace_", col)]]
            
            # Step 3: Apply the missing value strategy.
            if (option == "Mean") {
              if (is.numeric(processed[[col]])) {
                processed[[col]][is.na(processed[[col]])] <- mean(processed[[col]], na.rm = TRUE)
              } else {
                showNotification(paste("Column", col, "is not numeric for Mean calculation."), type = "error")
              }
            } else if (option == "Median") {
              if (is.numeric(processed[[col]])) {
                processed[[col]][is.na(processed[[col]])] <- median(processed[[col]], na.rm = TRUE)
              } else {
                showNotification(paste("Column", col, "is not numeric for Median calculation."), type = "error")
              }
            } else if (option == "Mode") {
              mode_value <- names(sort(table(processed[[col]]), decreasing = TRUE))[1]
              processed[[col]][is.na(processed[[col]])] <- mode_value
            } else if (option == "Specific Value" && !is.null(replace_value) && replace_value != "") {
              processed[[col]][is.na(processed[[col]])] <- replace_value
            } else if (option == "Remove") {
              processed <- processed[!is.na(processed[[col]]), ]
            }
          }
          stored_result(processed)
          removeModal()
        })
        
        
      } else if (option == "Remove duplicates") {
        processed <- processed[!duplicated(processed[selected_columns]), ]
        
      } else if (option == "Remove outliers") {
        for (col in selected_columns) {
          if (is.numeric(processed[[col]])) {
            Q1 <- quantile(processed[[col]], 0.25, na.rm = TRUE)
            Q3 <- quantile(processed[[col]], 0.75, na.rm = TRUE)
            IQR <- Q3 - Q1
            lower_bound <- Q1 - 1.5 * IQR
            upper_bound <- Q3 + 1.5 * IQR
            
            # Keep only rows where the column values are within the IQR range
            processed <- processed[processed[[col]] >= lower_bound & processed[[col]] <= upper_bound, , drop = FALSE]
          }
        }
      }
      
      stored_result(processed)
      process_clicked(TRUE)
    })
    
    output$processed_table <- renderDT({
      req(process_clicked())
      datatable(stored_result(), options = list(scrollX = TRUE))
    })
  })
  #_______________________________statistical analysis  
  
  observeEvent(input$statanalysis, {
    shinyjs::show("datastatanalysis")
    
    output$dynamic_test_selection <- renderUI({
      selectInput(
        "test_type",
        "Select Test to Perform:",
        choices = c("Select Test", "T test", "ANOVA","Repeated Measures ANOVA","Principal Component Analysis (PCA)"),
        selected = NULL,
        multiple = FALSE
      )
    })
    
    # Pop-up modal for test selection
    observeEvent(input$perform_test, {
      test_selected <- input$test_type
      if (test_selected == "T test") {
        # Show modal for T-test
        showModal(modalDialog(
          title = "T-test",
          
          # Custom layout for modal (simulating a sidebar and main content)
          fluidRow(
            column(4, # Sidebar width
                   wellPanel(
                     selectInput("test_type_choice", "Select T-test Type", 
                                 choices = c("One-sample t-test", "Two-sample t-test"), 
                                 selected = "One-sample"),
                     
                     # Column selection UI
                     uiOutput("column_select_ui"), 
                     
                     numericInput("mean1", "Mean (\u03bc\u2080) to compare with", value = 50),
                     radioButtons("hypothesis1", "Choose Hypothesis",
                                  choices = c("Two-sided" = "two.sided", "Greater" = "greater", "Less" = "less")),
                     actionButton("run_test", "Run Test"),
                     actionButton("cancel_test", "Cancel")
                   )
            ),
            
            # Main content area for results and plots
            column(8, 
                   tabsetPanel(
                     tabPanel("Result", DTOutput("t.test")),
                   )
            )
          ),
          
          easyClose = TRUE,
          footer = NULL,
          size = "l", # Optional: sizes available are "s", "m", "l"
          style = "width: 100%; height: 100%; max-width: none; max-height: none; margin: 0; padding: 0; overflow: auto;"
        ))
        
      } else if (test_selected == "ANOVA") {
        # Show modal for ANOVA
        showModal(modalDialog(
          title = "ANOVA",
          
          # Custom layout with Sidebar and Main Content (Result Tab Panel)
          fluidRow(
            column(4, # Sidebar width
                   wellPanel(
                     selectInput("anova_type", "Select ANOVA Type", 
                                 choices = c("One-way ANOVA", "Two-way ANOVA","One way MANOVA","Two way MANOVA"), 
                                 selected = "One-way ANOVA"),
                     
                     uiOutput("anova_column_select_ui"),
                     
                     
                     
                     actionButton("run_anova", "Run ANOVA"),
                     actionButton("cancel_anova", "Cancel")
                   )
            ),
            
            # Main Content Area (Tabs for displaying Results)
            column(8, 
                   tabsetPanel(
                     tabPanel("Result", DTOutput("anova_results")),
                     
                   )
            )
          ),
          
          easyClose = TRUE,
          footer = NULL,
          size = "l",  # Optional: sizes available are "s", "m", "l"
          style = "width: 100%; height: 100%; max-width: none; max-height: none; margin: 0; padding: 0; overflow: auto;"
        ))
        
      } else if (test_selected == "Repeated Measures ANOVA") {
        # Show modal for Repeated Measures ANOVA
        showModal(modalDialog(
          title = "Repeated Measures ANOVA",
          fluidRow(
            column(4,
                   wellPanel(
                     selectInput("rep_column", "Select Numeric Column ",
                                 choices = colnames(stored_result())),
                     selectInput("subject_column", "Select First Categorical Column",
                                 choices = colnames(stored_result())),
                     selectInput("time_column", "Select Second Categorical Column",
                                 choices = colnames(stored_result())),
                     actionButton("run_rep", "Run Analysis"),
                     actionButton("cancel_rep", "Cancel")
                   )
            ),
            column(8,
                   # You can keep this tabsetPanel if you want to show results in a separate tab
                   tabsetPanel(
                     tabPanel("Results", DTOutput("rep_anov"))  # This will display the results
                   )
            )
          ),
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          style = "width: 100%; height: 100%; max-width: none; max-height: none; margin: 0; padding: 0; overflow: auto;"
        ))
      } else if (test_selected == "Principal Component Analysis (PCA)") {
        # Show modal for PCA
        showModal(modalDialog(
          title = "Principal Component Analysis (PCA)",
          fluidRow(
            
            column(4,
                   wellPanel(
                     uiOutput("pca_action1"),  # First numeric column selection
                     uiOutput("pca_action2"),  # Second numeric column selection
                     actionButton("run_pca", "Run PCA"),
                     actionButton("cancel_pca", "Cancel")
                   )
                   
            ),
            column(8,
                   # Main Panel with Tabs for Result and Plot
                   
                   tabsetPanel(
                     tabPanel("PCA Results", DTOutput("components"),
                              DTOutput("explained_variance") ),
                     tabPanel(
                       "PCA Plot", 
                       plotlyOutput("pca_plot"),  # Display PCA Plot
                       uiOutput("pca_interpretation"),
                       downloadButton("download_pca", "Download PCA Biplot as PDF")  # Add Download Button
                     )
                   )
                   
            )
          ),
          easyClose = TRUE,
          footer = NULL,
          size = "l",  # Optional: sizes available are "s", "m", "l"
          style = "width: 100%; height: 100%; max-width: none; max-height: none; margin: 0; padding: 0; overflow: auto;"
        ))
        
      } 
    })
    
    # Helper function to check for constant values
    is_constant <- function(x) {
      length(unique(na.omit(x))) <= 1
    }
    #________________________ ttets 
    output$column_select_ui <- renderUI({
      req(stored_result())
      test_type <- input$test_type_choice
      numeric_columns <- names(stored_result())[sapply(stored_result(), is.numeric)]
      
      if (test_type == "One-sample t-test") {
        selectInput("ttest_column1", "Select Numeric Column for Analysis", 
                    choices = numeric_columns, 
                    multiple = FALSE)
      } else if (test_type == "Two-sample t-test") {
        tagList(
          selectInput("ttest_column2", "Select First Numeric Column for Analysis", 
                      choices = numeric_columns, 
                      multiple = FALSE),
          selectInput("ttest_column3", "Select Second Numeric Column for Analysis", 
                      choices = numeric_columns, 
                      multiple = FALSE)
        )
      }
    })
    
    
    observeEvent(input$run_test, {
      test_type <- input$test_type_choice
      col_1 <- input$ttest_column1
      col_2 <- input$ttest_column2
      col_3 <- input$ttest_column3
      mean_value <- input$mean1
      
      if (test_type == "One-sample t-test") {
        req(stored_result())
        column_data <- stored_result()[[col_1]]
        
        if (!is.numeric(column_data)) {
          showNotification("Error: Selected column is not numeric.", type = "error")
          return()
        }
        
        if (is_constant(column_data)) {
          showNotification("Error: Selected column has constant values. please select another column.", type = "error")
          return()
        }
        
        t_res1 <- t.test(column_data, mu = mean_value, alternative = input$hypothesis1)
        
        
        
        t_res_table1 <- data.frame(
          T_Statistic = round(t_res1$statistic, 6),
          P_Value = t_res1$p.value,
          Estimated_Mean = round(t_res1$estimate, 6),
          Confidence_Interval = if (input$hypothesis1 == "two.sided") {
            paste0("(", round(t_res1$conf.int[1], 6), ", ", round(t_res1$conf.int[2], 6), ")")
          } else if (input$hypothesis1 == "greater") {
            paste0("Lower bound: ", round(t_res1$conf.int[1], 6))
          } else {
            paste0("Upper bound: ", round(t_res1$conf.int[2], 6))
          },
          Degrees_of_Freedom = t_res1$parameter
        )
        
        results$t_res_table1 <- t_res_table1
        
        output$t.test <- DT::renderDT({
          datatable(t_res_table1, extensions = 'Buttons',
                    options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE))
        })
        
        # History
        new_entry <- data.frame(
          TestName = "One-Sample T-Test",
          Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View",
                                           onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete",
                                             onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          stringsAsFactors = FALSE
        )
        results$history <- rbind(results$history, new_entry)
        
      } else if (test_type == "Two-sample t-test") {
        column_data1 <- stored_result()[[col_2]]
        column_data2 <- stored_result()[[col_3]]
        
        if (!is.numeric(column_data1) || !is.numeric(column_data2)) {
          showNotification("Error: One or both selected columns are not numeric.", type = "error")
          return()
        }
        
        if (is_constant(column_data1) || is_constant(column_data2)) {
          showNotification("Error: One or both columns have constant values. please select another column.", type = "error")
          return()
        }
        
        t_res <- t.test(column_data1, column_data2, alternative = input$hypothesis1)
        
        
        t_res_table2 <- data.frame(
          T_Statistic = round(t_res$statistic, 6),
          P_Value = t_res$p.value,
          Estimated_Mean = round(t_res$estimate, 6),
          Confidence_Interval = if (input$hypothesis1 == "two.sided") {
            paste0("(", round(t_res$conf.int[1], 6), ", ", round(t_res$conf.int[2], 6), ")")
          } else if (input$hypothesis1 == "greater") {
            paste0("Lower bound: ", round(t_res$conf.int[1], 6))
          } else {
            paste0("Upper bound: ", round(t_res$conf.int[2], 6))
          },
          Degrees_of_Freedom = t_res$parameter
        )
        
        results$t_res_table2 <- t_res_table2
        
        output$t.test <- DT::renderDT({
          datatable(t_res_table2, extensions = 'Buttons',
                    options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE))
        })
        
        # History
        new_entry <- data.frame(
          TestName = "Two-Sample T-Test",
          Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View",
                                           onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete",
                                             onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          stringsAsFactors = FALSE
        )
        results$history <- dplyr::distinct(rbind(results$history, new_entry))
      }
    })
    
    # ________________anova
    
    output$anova_column_select_ui <- renderUI({
      if (input$anova_type == "One-way ANOVA") {
        tagList(
          selectInput("oneway_dv", "Select the Numeric Column",
                      choices = colnames(stored_result()), 
                      multiple = FALSE),
          selectInput("oneway_factor", "Select the Categorical Column",
                      choices = colnames(stored_result()), 
                      multiple = FALSE)
        )
      } else if (input$anova_type == "Two-way ANOVA") {
        tagList(
          selectInput("twoway_dv", "Select the Numeric Column",
                      choices = colnames(stored_result()), 
                      multiple = FALSE),
          selectInput("twoway_factor1", "Select the First Categorical Column",
                      choices = colnames(stored_result()), 
                      multiple = FALSE),
          selectInput("twoway_factor2", "Select the Second Categorical Column",
                      choices = colnames(stored_result()), 
                      multiple = FALSE)
        )
      } else if (input$anova_type == "One way MANOVA") {
        tagList(
          selectInput("manova_oneway_dv", "Select the Numeric Columns",
                      choices =colnames(stored_result()), multiple = TRUE),
          selectInput("manova_oneway_factor", "Select the Categorical Column",
                      choices =colnames(stored_result()), multiple = FALSE)
        )
      } else if (input$anova_type == "Two way MANOVA") {
        tagList(
          selectInput("manova_twoway_dv", "Select the Numeric Columns",
                      choices =colnames(stored_result()), multiple = TRUE),
          selectInput("manova_twoway_factor1", "Select the First Categorical Column",
                      choices =colnames(stored_result()), multiple = FALSE),
          selectInput("manova_twoway_factor2", "Select the Second Categorical Column",
                      choices =colnames(stored_result()), multiple = FALSE)
        )
      }
    })
    
    observeEvent(input$run_anova, {
      req(stored_result())
      df <- stored_result()
      
      # --- ONE-WAY ANOVA ---
      if (input$anova_type == "One-way ANOVA") {
        req(input$oneway_dv, input$oneway_factor)
        
        if (!is.numeric(df[[input$oneway_dv]])) {
          showNotification("Error: Dependent variable must be numeric for One-Way ANOVA.", type = "error")
          return()
        }
        if (is_constant(df[[input$oneway_dv]])) {
          showNotification("Error: Dependent variable has constant values. Please select another column.", type = "error")
          return()
        }
        if (is.numeric(df[[input$oneway_factor]])) {
          showNotification("Error: Factor must be categorical (not numeric).", type = "error")
          return()
        }
        if (is.character(df[[input$oneway_factor]])) {
          df[[input$oneway_factor]] <- factor(df[[input$oneway_factor]])
        }
        if (!is.factor(df[[input$oneway_factor]])) {
          showNotification("Error: Please select a categorical factor.", type = "error")
          return()
        }
        df[[input$oneway_factor]] <- as.factor(df[[input$oneway_factor]])
        if (length(unique(df[[input$oneway_factor]])) > 10) {
          showNotification("Error: Selected factor has more than 10 unique levels. Please choose a simpler categorical variable.", type = "error")
          return()
        }
        
        formula <- as.formula(paste(input$oneway_dv, "~", input$oneway_factor))
        res <- aov(formula, data = df)
        aov_res_table1 <- as.data.frame(summary(res)[[1]])
        colnames(aov_res_table1) <- c("Degrees of Freedom (DF)", "Sum of Squares (SS)", "Mean Squares (MS)", "F Statistic", "P Value")
        results$aov_res_table1 <- aov_res_table1
        
        output$anova_results <- DT::renderDT({
          datatable(aov_res_table1, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE))
        })
        
        new_entry <- data.frame(
          TestName = "One-Way ANOVA",
          Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View", onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete", onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          stringsAsFactors = FALSE
        )
        results$history <- rbind(results$history, new_entry)
        
        # --- TWO-WAY ANOVA ---
      } else if (input$anova_type == "Two-way ANOVA") {
        req(input$twoway_dv, input$twoway_factor1, input$twoway_factor2)
        
        if (!is.numeric(df[[input$twoway_dv]])) {
          showNotification("Error: Dependent variable must be numeric for Two-Way ANOVA.", type = "error")
          return()
        }
        if (is_constant(df[[input$twoway_dv]])) {
          showNotification("Error: Dependent variable has constant values. Please select another column.", type = "error")
          return()
        }
        if (is.numeric(df[[input$twoway_factor1]]) || is.numeric(df[[input$twoway_factor2]])) {
          showNotification("Error: Both factors must be categorical (not numeric) for Two-Way ANOVA.", type = "error")
          return()
        }
        df[[input$twoway_factor1]] <- as.factor(df[[input$twoway_factor1]])
        df[[input$twoway_factor2]] <- as.factor(df[[input$twoway_factor2]])
        
        if (is_constant(df[[input$twoway_factor1]])) {
          showNotification("Error: First factor has constant values. Please select another column.", type = "error")
          return()
        }
        if (is_constant(df[[input$twoway_factor2]])) {
          showNotification("Error: Second factor has constant values. Please select another column.", type = "error")
          return()
        }
        
        df[[input$twoway_factor1]] <- as.factor(df[[input$twoway_factor1]])
        df[[input$twoway_factor2]] <- as.factor(df[[input$twoway_factor2]])
        
        if (length(unique(df[[input$twoway_factor1]])) > 10 || length(unique(df[[input$twoway_factor2]])) > 10) {
          showNotification("Error: One or both factors have more than 10 unique levels.", type = "error")
          return()
        }
        
        res <- aov(df[[input$twoway_dv]] ~ df[[input$twoway_factor1]] * df[[input$twoway_factor2]])
        res.table <- anova(res)
        rownames(res.table)[1:3] <- c(input$twoway_factor1, input$twoway_factor2, paste0(input$twoway_factor1, " : ", input$twoway_factor2))
        colnames(res.table) <- c("Degree of Freedom (DF)", "Sum of Squares (SS)", "Mean Squares (MS)", "F Statistic", "P Value")
        results$aov_res_table2 <- res.table
        
        output$anova_results <- DT::renderDT({
          datatable(res.table, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE))
        })
        
        new_entry <- data.frame(
          TestName = "Two-Way ANOVA",
          Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View", onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete", onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          stringsAsFactors = FALSE
        )
        results$history <- rbind(results$history, new_entry)
        
        # --- ONE-WAY MANOVA ---
      } else if (input$anova_type == "One way MANOVA") {
        req(input$manova_oneway_dv, input$manova_oneway_factor)
        
        if (length(input$manova_oneway_dv) < 2) {
          showNotification("Please select at least two numeric dependent variables for MANOVA.", type = "error")
          return()
        }
        if (any(!sapply(df[, input$manova_oneway_dv, drop = FALSE], is.numeric))) {
          showNotification("Please select numeric dependent variables for MANOVA.", type = "error")
          return()
        }
        if (any(sapply(df[, input$manova_oneway_dv, drop = FALSE], is_constant))) {
          showNotification("Error: One or more dependent variables are constant. Cannot perform MANOVA.", type = "error")
          return()
        }
        if (is.numeric(df[[input$manova_oneway_factor]])) {
          showNotification("Error: Factor must be categorical (not numeric) for MANOVA.", type = "error")
          return()
        }
        
        df[[input$manova_oneway_factor]] <- as.factor(df[[input$manova_oneway_factor]])
        manova_formula <- as.formula(paste("cbind(", paste(input$manova_oneway_dv, collapse = ", "), ") ~", input$manova_oneway_factor))
        manova_res <- manova(manova_formula, data = df)
        result <- summary(manova_res)
        
        if (length(result) < 1 || is.null(result[[1]])) {
          showNotification("Error in MANOVA results. Please check your inputs.", type = "error")
          return()
        }
        df[[input$manova_oneway_factor]] <- as.factor(df[[input$manova_oneway_factor]])
        if (length(unique(df[[input$manova_oneway_factor]])) > 10) {
          showNotification("Error: Selected factor has more than 10 unique levels.", type = "error")
          return()
        }
        
        result_df <- as.data.frame(result$stats)
        colnames(result_df) <- c("Degrees of Freedom (Df)", "Pillai's Trace", "Approximate F-statistic", "Numerator Degrees of Freedom (num Df)", "Denominator Degrees of Freedom (den Df)", "p-value (Pr(>F))")
        results$manova_res_table1 <- result_df
        
        output$anova_results <- DT::renderDT({
          datatable(result_df, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE))
        })
        
        new_entry <- data.frame(
          TestName = "One-Way MANOVA",
          Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View", onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete", onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          stringsAsFactors = FALSE
        )
        results$history <- rbind(results$history, new_entry)
        
        # --- TWO-WAY MANOVA ---
      } else if (input$anova_type == "Two way MANOVA") {
        req(input$manova_twoway_dv, input$manova_twoway_factor1, input$manova_twoway_factor2)
        
        if (length(input$manova_twoway_dv) < 2) {
          showNotification("Please select at least two numeric dependent variables for MANOVA.", type = "error")
          return()
        }
        if (any(!sapply(df[, input$manova_twoway_dv, drop = FALSE], is.numeric))) {
          showNotification("Please select numeric dependent variables for MANOVA.", type = "error")
          return()
        }
        if (any(sapply(df[, input$manova_twoway_dv, drop = FALSE], is_constant))) {
          showNotification("Error: One or more dependent variables are constant. Cannot perform MANOVA.", type = "error")
          return()
        }
        if (is.numeric(df[[input$manova_twoway_factor1]]) || is.numeric(df[[input$manova_twoway_factor2]])) {
          showNotification("Error: Both factors must be categorical (not numeric) for MANOVA.", type = "error")
          return()
        }
        
        df[[input$manova_twoway_factor1]] <- as.factor(df[[input$manova_twoway_factor1]])
        df[[input$manova_twoway_factor2]] <- as.factor(df[[input$manova_twoway_factor2]])
        manova_formula <- as.formula(paste("cbind(", paste(input$manova_twoway_dv, collapse = ", "), ") ~", input$manova_twoway_factor1, "*", input$manova_twoway_factor2))
        manova_res <- manova(manova_formula, data = df)
        result <- summary(manova_res)
        
        if (length(result) < 1 || is.null(result[[1]])) {
          showNotification("Error in MANOVA results. Please check your inputs.", type = "error")
          return()
        }
        
        df[[input$manova_twoway_factor1]] <- as.factor(df[[input$manova_twoway_factor1]])
        df[[input$manova_twoway_factor2]] <- as.factor(df[[input$manova_twoway_factor2]])
        
        if (length(unique(df[[input$manova_twoway_factor1]])) > 10 || length(unique(df[[input$manova_twoway_factor2]])) > 10) {
          showNotification("Error: One or both factors have more than 10 unique levels.", type = "error")
          return()
        }
        
        result_df <- as.data.frame(result$stats)
        colnames(result_df) <- c("Degrees of Freedom (Df)", "Pillai's Trace", "Approximate F-statistic", "Numerator Degrees of Freedom (num Df)", "Denominator Degrees of Freedom (den Df)", "p-value (Pr(>F))")
        results$manova_res_table2 <- result_df
        
        output$anova_results <- DT::renderDT({
          datatable(result_df, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE))
        })
        
        new_entry <- data.frame(
          TestName = "Two-Way MANOVA",
          Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View", onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete", onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
          stringsAsFactors = FALSE
        )
        results$history <- rbind(results$history, new_entry)
      }
    })
    
    #___________________________________________repeted mesure anova _________________________
    
    observeEvent(input$run_rep, {
      req(stored_result())
      df <- stored_result()
      
      req(input$rep_column, input$subject_column, input$time_column)
      
      if (!all(c(input$rep_column, input$subject_column, input$time_column) %in% colnames(df))) {
        showNotification("One or more selected columns do not exist in the dataset", type = "error")
        return()
      }
      
      if (!is.numeric(df[[input$rep_column]])) {
        showNotification("The dependent variable must be numeric", type = "error")
        return()
      }
      
      if (is_constant(df[[input$rep_column]])) {
        showNotification("Error: Dependent variable has constant values. Cannot perform Repeated Measures ANOVA.", type = "error")
        return()
      }
      
      df[[input$subject_column]] <- as.factor(df[[input$subject_column]])
      df[[input$time_column]] <- as.factor(df[[input$time_column]])
      
      # New Checks: Prevent numeric selection for categorical variables
      if (is.numeric(df[[input$subject_column]])) {
        showNotification("The subject column must be categorical (not numeric)", type = "error")
        return()
      }
      if (is.numeric(df[[input$time_column]])) {
        showNotification("The time column must be categorical (not numeric)", type = "error")
        return()
      }
      
      if (is_constant(df[[input$subject_column]])) {
        showNotification("Error: Subject column has constant values. Cannot perform Repeated Measures ANOVA.", type = "error")
        return()
      }
      if (is_constant(df[[input$time_column]])) {
        showNotification("Error: Time column has constant values. Cannot perform Repeated Measures ANOVA.", type = "error")
        return()
      }
      
      model <- aov(as.formula(paste(input$rep_column, "~ factor(", input$time_column, ") + Error(factor(", input$subject_column, "))")), data = df)
      res_summary <- summary(model)
      
      if (length(res_summary) < 2 || is.null(res_summary[[2]][[1]])) {
        showNotification("Error generating ANOVA summary. Check data structure.", type = "error")
        return()
      }
      
      rep_measure1 <- as.data.frame(res_summary[[2]][[1]])
      colnames(rep_measure1) <- c("Degrees of Freedom (DF)", "Sum of Squares (Sum Sq)", "Mean Squares (Mean Sq)", "F-value", "p-value")
      results$rep_anova <- rep_measure1
      
      output$rep_anov <- DT::renderDT({
        datatable(
          rep_measure1,
          extensions = 'Buttons',
          options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE)
        )
      })
      
      new_entry <- data.frame(
        TestName = "Repeated Measures ANOVA",
        Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        View = as.character(actionButton(paste0("view_", nrow(results$history) + 1), "View", onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});"))),
        Delete = as.character(actionButton(paste0("delete_", nrow(results$history) + 1), "Delete", onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});"))),
        stringsAsFactors = FALSE
      )
      results$history <- rbind(results$history, new_entry)
    })
    
    #_____________pca
    # Dynamic column selection UI for PCA
    output$pca_action1 <- renderUI({
      req(stored_result())
      print(str(stored_result()))
      selectInput("pca_column1", "Select First Numeric Column for PCA", choices = colnames(stored_result()))
    })
    
    output$pca_action2 <- renderUI({
      req(stored_result())
      selectInput("pca_column2", "Select Second Numeric Column for PCA", choices = colnames(stored_result()))
    })
    
    observeEvent(input$run_pca, {
      req(stored_result())
      
      if (!(input$pca_column1 %in% colnames(stored_result())) || 
          !(input$pca_column2 %in% colnames(stored_result()))) {
        showNotification("One or both of the selected columns do not exist in the dataset.", 
                         type = "error")
        return()
      }
      
      pca_data <- stored_result() %>%
        select(all_of(c(input$pca_column1, input$pca_column2))) %>%
        na.omit()
      
      if (!all(sapply(pca_data, is.numeric))) {
        showNotification("Both selected columns must be numeric for PCA.", type = "error")
        return()
      }
      if (any(sapply(pca_data, is_constant))) {
        showNotification("One or both selected columns have constant values. PCA requires variability.", 
                         type = "error")
        return()
      }
      
      # Perform PCA
      pca_result <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)
      components <- as.data.frame(pca_result$ind$coord)
      colnames(components) <- c("Dimension 1", "Dimension 2")
      explained_variance <- pca_result$eig
      rownames(explained_variance) <- paste("Component", 1:nrow(explained_variance))
      
      results$pca_components <- components
      results$pca_explained_variance <- explained_variance
      
      output$components <- renderDT({
        datatable(components, extensions = 'Buttons', 
                  options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE))
      })
      
      output$explained_variance <- renderDT({
        datatable(explained_variance, extensions = 'Buttons', 
                  options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE))
      })
      
      correlation_value <- cor(pca_data)[1, 2]
      angle_radians <- acos(correlation_value)
      angle_degrees <- angle_radians * 180 / pi
      
      # Interpret based on the angle
      if (angle_degrees < 90) {
        angle_interpretation <- "a positive correlation"
      } else if (angle_degrees > 90) {
        angle_interpretation <- "a negative correlation"
      } else {
        angle_interpretation <- "no correlation"
      }
      
      # Create bullet-point interpretation text for ease of understanding
      interpretation_text <- paste0(
        "For the PCA analysis of columns '", input$pca_column1, "' and '", input$pca_column2, "', the computed correlation is ", 
        round(correlation_value, 2), " with an angle of ", round(angle_degrees, 1), "°. ",
        "An angle less than 90° indicates a positive correlation, around 90° indicates no correlation, and greater than 90° indicates a negative correlation. ",
        "Therefore, this PCA suggests that there is ", angle_interpretation, "."
      )
      results$pca_interpretation_text <- interpretation_text
      
      # Render the interpretation in the UI (below the plot)
      output$pca_interpretation <- renderUI({
        req(results$pca_explained_variance)
        tagList(
          h4("Interpretation"),
          HTML(gsub("\n", "<br>", interpretation_text))
        )
      })
      
      output$pca_plot <- renderPlotly({
        p <- fviz_pca_biplot(pca_result,
                             repel = TRUE,
                             title = paste("PCA Biplot:", input$pca_column1, "vs", input$pca_column2),
                             xlab = paste0("Principal Component1 (", round(pca_result$eig[1,2], 1), "%)"),
                             ylab = paste0("Principal Component2 (", round(pca_result$eig[2,2], 1), "%)")) +
          theme(plot.title = element_text(hjust = 0.5))
        
        plotly_pca <- ggplotly(p) %>%
          layout(
            title = list(
              text = paste("PCA Biplot:", input$pca_column1, "vs", input$pca_column2),
              x = 0.5,
              xanchor = "center"
            ),
            showlegend = TRUE
          )
        
        # Store the static plot for download
        results$pca_plot_static <- p
        
        plotly_pca
      })
      
      output$download_pca <- downloadHandler(
        filename = function() {
          paste("PCA_Biplot_", format(Sys.Date(), "%Y-%m-%d"), ":", 
                format(Sys.time(), "%H-%M-%S"), ".pdf", sep = "")
        },
        content = function(file) {
          if (is.null(results$pca_plot_static)) {
            showNotification("No PCA plot available to download", type = "error")
            return()
          }
          # Save the static PCA plot as a temporary PNG file
          temp_file <- tempfile(fileext = ".png")
          ggsave(temp_file, plot = results$pca_plot_static, device = "png", height = 8, width = 11)
          
          # Open a PDF device and arrange the plot and interpretation text vertically
          pdf(file, height = 8.0, width = 11.0)
          img <- grid::rasterGrob(png::readPNG(temp_file), width = unit(1, "npc"), height = unit(1, "npc"))
          interpretation_grob <- grid::textGrob(
            interpretation_text, 
            gp = grid::gpar(fontsize = 10),
            just = "left",
            x = unit(0, "npc")
          )
          gridExtra::grid.arrange(img, interpretation_grob, ncol = 1, heights = c(4, 1))
          dev.off()
        }
      )
      
      new_entry <- data.frame(
        TestName = "Principal Component Analysis (PCA)",
        Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        View = as.character(actionButton(
          paste0("view_", nrow(results$history) + 1), 
          "View", 
          onclick = paste0("Shiny.setInputValue('view',", nrow(results$history) + 1, ", {priority: 'event'});")
        )),
        Delete = as.character(actionButton(
          paste0("delete_", nrow(results$history) + 1), 
          "Delete", 
          onclick = paste0("Shiny.setInputValue('delete',", nrow(results$history) + 1, ", {priority: 'event'});")
        )),
        stringsAsFactors = FALSE
      )
      results$history <- rbind(results$history, new_entry)
    })
    
    
    
    observeEvent(input$cancel_test, {
      removeModal()  # Close the T-test modal
    })
    
    observeEvent(input$cancel_anova, {
      removeModal()  # Close the ANOVA modal
    })
    
    observeEvent(input$cancel_pca, {
      removeModal()  # Close the PCA modal
    })
    
    observeEvent(input$cancel_rep, {
      removeModal()  # Close the rep modal
    })
    
    
    #________________________ plots_____________________________ 
    observeEvent(input$plot, {
      
      # Update chart type UI
      output$dynamic_plot_selection <- renderUI({
        selectInput(
          "chart_type",
          "Select Control Chart and Bar Chart Type",
          choices = c("X-Bar Chart", "R Chart", "p Chart", "np Chart", "c Chart", "u Chart", 
                      "Grouped Bar Chart", "Stacked Bar Chart", "Stretched Bar Chart", "HeatMap"),
          selected = NULL,
          multiple = FALSE
        )
      })
      
      # Update column selection UI based on chart type
      observeEvent(input$chart_type, {
        req(stored_result())  # Ensure that stored_result() is available
        cols <- colnames(stored_result())
        
        output$column_ui_selection1 <- renderUI({
          if (input$chart_type %in% c("X-Bar Chart", "R Chart", "p Chart", "np Chart", "c Chart", "u Chart")) {
            selectInput("column3", "Select Column:", choices = cols)
            
          } else if (input$chart_type == "Grouped Bar Chart") {
            tagList(
              selectInput("xcol", "Select X-axis (Categorical)", choices = cols),
              selectInput("ycol", "Select Y-axis (Numeric)", choices = cols),
              selectInput("fillcol", "Select Fill (Categorical)", choices = cols)
            )
            
          }  else if (input$chart_type == "Stacked Bar Chart") {
            tagList(
              selectInput("xcol1", "Select X-axis (Categorical)", choices = cols),
              selectInput("ycol1", "Select Y-axis (Numeric)", choices = cols),
              selectInput("fillcol1", "Select Fill (Categorical)", choices = cols)
            )
            
          } else if (input$chart_type == "Stretched Bar Chart") {
            tagList(
              selectInput("xcol2", "Select X-axis (Categorical)", choices = cols),
              selectInput("ycol2", "Select Y-axis (Numeric)", choices = cols)
            )
            
          } else if (input$chart_type == "HeatMap") {
            tagList(
              selectInput("rowcol", "Select Row (Categorical)", choices = cols),
              selectInput("colcol", "Select Column (Categorical)", choices = cols),
              selectInput("valuecol", "Select Value (Numeric)", choices = cols)
            )
          }
        })
      })
      
      
      observeEvent(input$chart_type, {
        # Show UCL and LCL sliders only for control charts
        if (input$chart_type %in% c("X-Bar Chart", "R Chart", "p Chart", "np Chart", "c Chart", "u Chart")) {
          shinyjs::show("ucl_slider")
          shinyjs::show("lcl_slider")
        } else {
          shinyjs::hide("ucl_slider")
          shinyjs::hide("lcl_slider")
        }
      })
      
      observeEvent(input$column3, {
        req(input$column3)  # Ensure a column is selected
        column_data <- stored_result()[[input$column3]]  # Get data from the selected column
        
        # Ensure the column is numeric
        if (!is.numeric(column_data)) {
          showNotification("Please select a numeric column to generate chart", type = "error")
          return()  # Stop further execution
        }
        
        # Remove NAs and convert to numeric
        column_data <- na.omit(as.numeric(column_data))
        
        # Dynamically update the slider range based on the column's values
        output$ucl_slider <- renderUI({
          sliderInput("ucl_slider", "UCL", 
                      min = min(column_data, na.rm = TRUE), 
                      max = max(column_data, na.rm = TRUE), 
                      value = max(column_data, na.rm = TRUE))  # UCL slider based on the column data range
        })
        
        output$lcl_slider <- renderUI({
          sliderInput("lcl_slider", "LCL", 
                      min = min(column_data, na.rm = TRUE), 
                      max = max(column_data, na.rm = TRUE), 
                      value = min(column_data, na.rm = TRUE))  # LCL slider based on the column data range
        })
      })
      
      observe({
        req(input$chart_type)  # Ensure input$chart_type is not NULL
        
        # Check if input$chart_type is empty before updating the button
        if (input$chart_type == "") {
          updateActionButton(session, "generate_chart", label = "Generate chart")
        } else if (input$chart_type %in% c("X-Bar Chart", "R Chart", "p Chart", "np Chart", "c Chart", "u Chart")) {
          updateActionButton(session, "generate_chart", label = "Quality Control Chart")
        } else {
          updateActionButton(session, "generate_chart", label = "PLOT")
        }
      })
      
      # Create a reactive value to store the plot
      generated_plot <- reactiveVal(NULL)
      
      observeEvent(input$generate_chart, {
        req(input$column3)
        chart_type <- input$chart_type
        
        # Step 1: Get the column data and check if it's numeric.
        column_data <- stored_result()[[input$column3]]
        if (!is.numeric(column_data)) {
          showNotification("Please select a numeric column to generate the chart.", type = "error")
          return()
        }
        
        # Step 2: Convert data to numeric and remove NAs.
        column_data <- na.omit(as.numeric(column_data))
        
        # Step 3: Get UCL and LCL slider values (they must exist).
        req(input$ucl_slider, input$lcl_slider)
        ucl <- input$ucl_slider
        lcl <- input$lcl_slider
        
        # Step 4: If data length is odd, remove the last element.
        if (length(column_data) %% 2 != 0) {
          column_data <- column_data[-length(column_data)]
        }
        
        # Build the plot data ensuring all vectors have the same length.
        plot_data <- data.frame(
          Sample = seq_along(column_data),
          Measurement = column_data,
          UCL = rep(ucl, length(column_data)),
          LCL = rep(lcl, length(column_data)),
          Mean = rep(mean(column_data), length(column_data))
        )
        
        # Generate the chart (example for X-Bar Chart)
        if (chart_type == "X-Bar Chart") {
          subgroup_size <- 5
          subgroups <- split(column_data, ceiling(seq_along(column_data) / subgroup_size))
          xbar_values <- sapply(subgroups, mean)
          
          p <- plot_ly(data = data.frame(Sample = seq_along(xbar_values), Measurement = xbar_values),
                       x = ~Sample, y = ~Measurement,
                       type = 'scatter', mode = 'lines+markers', name = 'X-Bar') %>%
            add_trace(y = rep(mean(column_data), length(xbar_values)),
                      mode = 'lines', name = 'Mean', line = list(dash = 'dash', color = 'blue')) %>%
            add_trace(y = rep(ucl, length(xbar_values)),
                      mode = 'lines', name = 'UCL', line = list(dash = 'dot', color = 'red')) %>%
            add_trace(y = rep(lcl, length(xbar_values)),
                      mode = 'lines', name = 'LCL', line = list(dash = 'dot', color = 'green')) %>%
            layout(title = paste(chart_type, input$column3),
                   xaxis = list(title = "Subgroup"),
                   yaxis = list(title = "Measurement"),
                   legend = list(x = 0.1, y = 0.9)) %>%
            config(toImageButtonOptions = list(
              format = "png",
              filename = paste0("X-Bar_Chart_", format(Sys.time(), "%Y%m%d_%H%M%S"))
            ))
          
          # Render the chart.
          output$control_chart <- renderPlotly({ p })
        }
      })
      
        } else if (chart_type == "R Chart") {
          subgroup_size <- 5
          subgroups <- split(column_data, ceiling(seq_along(column_data) / subgroup_size))
          r_values <- sapply(subgroups, function(subgroup) max(subgroup) - min(subgroup))
          
          # Calculate the average range (R-bar) as the Center Line
          center <- mean(r_values)
          
          # Standard constants for subgroup size of 5:
          D3 <- 0
          D4 <- 2.114
          
          # Calculate control limits
          ucl <- D4 * center
          lcl <- D3 * center  # This will be 0
          
          p <- plot_ly(data = data.frame(Sample = seq_along(r_values), Measurement = r_values),
                       x = ~Sample, y = ~Measurement, type = 'scatter', mode = 'lines+markers', name = 'Range') %>%
            add_trace(y = rep(ucl, length(r_values)), mode = 'lines', name = 'UCL',
                      line = list(dash = 'dot', color = 'red')) %>%
            add_trace(y = rep(lcl, length(r_values)), mode = 'lines', name = 'LCL',
                      line = list(dash = 'dot', color = 'green')) %>%
            add_trace(y = rep(center, length(r_values)), mode = 'lines', name = 'Center Line',
                      line = list(dash = 'dash', color = 'blue')) %>%
            layout(title = paste(chart_type, input$column3),
                   xaxis = list(title = "Subgroup"),
                   yaxis = list(title = "Range"),
                   legend = list(x = 0.1, y = 0.9)) %>%
            # Configure PNG download button with timestamped filename
            config(toImageButtonOptions = list(
              format = "png",
              filename = paste0("R_Chart_", format(Sys.time(), "%Y%m%d_%H%M%S"))
            ))
          
          
        } else if (chart_type == "p Chart") {
          # Mark items as defective if below the overall mean (example criterion)
          defectives <- column_data < mean(column_data)
          
          # Calculate proportion defective per subgroup (with subgroup size 5)
          p_values <- tapply(defectives, ceiling(seq_along(defectives) / 5), mean)
          
          # Calculate the Center Line as the average proportion (p-bar)
          p_bar <- mean(p_values)
          
          # Sample size per subgroup (n)
          n <- 5
          
          # Calculate control limits for a p chart
          ucl <- p_bar + 3 * sqrt(p_bar * (1 - p_bar) / n)
          lcl <- p_bar - 3 * sqrt(p_bar * (1 - p_bar) / n)
          lcl <- ifelse(lcl < 0, 0, lcl)  # Set LCL to 0 if it is negative
          
          
          p <- plot_ly(data = data.frame(Sample = seq_along(p_values), Measurement = p_values),
                       x = ~Sample, y = ~Measurement, type = 'scatter', mode = 'lines+markers', name = 'p') %>%
            add_trace(y = rep(ucl, length(p_values)), mode = 'lines', name = 'UCL',
                      line = list(dash = 'dot', color = 'red')) %>%
            add_trace(y = rep(lcl, length(p_values)), mode = 'lines', name = 'LCL',
                      line = list(dash = 'dot', color = 'green')) %>%
            add_trace(y = rep(p_bar, length(p_values)), mode = 'lines', name = 'Center Line',
                      line = list(dash = 'dash', color = 'blue')) %>%
            layout(title = paste(chart_type, input$column3),
                   xaxis = list(title = "Subgroup"),
                   yaxis = list(title = "Proportion of Defectives"),
                   legend = list(x = 0.1, y = 0.9)) %>%
            config(toImageButtonOptions = list(
              format = "png",
              filename = paste0("p_Chart_", format(Sys.time(), "%Y%m%d_%H%M%S"))
            ))
          
          
        } else if (chart_type == "np Chart") {
          # Create np Chart: calculate the number of defectives for each subgroup
          defectives <- column_data < mean(column_data)  # mark items as defective if below overall mean
          np_values <- tapply(defectives, ceiling(seq_along(defectives) / 5), sum)  # Sum of defectives per subgroup
          
          # Calculate the Center Line (np-bar) as the average number of defectives
          center <- mean(np_values)
          
          # Assume constant subgroup size n
          n <- 5
          # p_bar is the average proportion defective
          p_bar <- center / n
          
          # Calculate the standard deviation for the np chart
          sigma_np <- sqrt(n * p_bar * (1 - p_bar))
          
          # Calculate control limits
          ucl <- center + 3 * sigma_np
          lcl <- center - 3 * sigma_np
          lcl <- ifelse(lcl < 0, 0, lcl)
          
          p <- plot_ly(data = data.frame(Sample = seq_along(np_values), Measurement = np_values),
                       x = ~Sample, y = ~Measurement, type = 'scatter', mode = 'lines+markers', name = 'np') %>%
            add_trace(y = rep(ucl, length(np_values)), mode = 'lines', name = 'UCL',
                      line = list(dash = 'dot', color = 'red')) %>%
            add_trace(y = rep(lcl, length(np_values)), mode = 'lines', name = 'LCL',
                      line = list(dash = 'dot', color = 'green')) %>%
            add_trace(y = rep(center, length(np_values)), mode = 'lines', name = 'Center Line',
                      line = list(dash = 'dash', color = 'blue')) %>%
            layout(title = paste(chart_type, input$column3),
                   xaxis = list(title = "Subgroup"),
                   yaxis = list(title = "Number of Defectives"),
                   legend = list(x = 0.1, y = 0.9)) %>%
            config(toImageButtonOptions = list(
              format = "png",
              filename = paste0("np_Chart_", format(Sys.time(), "%Y%m%d_%H%M%S"))
            ))
          
          
        } else if (chart_type == "c Chart") {
          # Create c Chart: calculate the count of defects per subgroup
          defects <- column_data  # Example for defects count
          c_values <- tapply(defects, ceiling(seq_along(defects) / 5), sum)  # Count of defects per subgroup
          
          # Calculate the Center Line (c-bar) as the average defects count
          center <- mean(c_values)
          
          # Calculate control limits for a c Chart
          sigma <- sqrt(center)
          ucl <- center + 3 * sigma
          lcl <- center - 3 * sigma
          lcl <- ifelse(lcl < 0, 0, lcl)  # Ensure LCL is not negative
          
          
          p <- plot_ly(data = data.frame(Sample = seq_along(c_values), Measurement = c_values),
                       x = ~Sample, y = ~Measurement, type = 'scatter', mode = 'lines+markers', name = 'c') %>%
            add_trace(y = rep(ucl, length(c_values)), mode = 'lines', name = 'UCL',
                      line = list(dash = 'dot', color = 'red')) %>%
            add_trace(y = rep(lcl, length(c_values)), mode = 'lines', name = 'LCL',
                      line = list(dash = 'dot', color = 'green')) %>%
            add_trace(y = rep(center, length(c_values)), mode = 'lines', name = 'Center Line',
                      line = list(dash = 'dash', color = 'blue')) %>%
            layout(title = paste(chart_type, input$column3),
                   xaxis = list(title = "Subgroup"),
                   yaxis = list(title = "Defects Count"),
                   legend = list(x = 0.1, y = 0.9)) %>%
            config(toImageButtonOptions = list(
              format = "png",
              filename = paste0("c_Chart_", format(Sys.time(), "%Y%m%d_%H%M%S"))
            ))
          
          
        } else if (chart_type == "u Chart") {
          # Create u Chart: calculate the defects per unit for each subgroup
          defects <- column_data  # Example: defect counts per unit
          u_values <- tapply(defects, ceiling(seq_along(defects) / 5), mean)  # Defects per unit per subgroup
          
          # Calculate the center line (u-bar): average defects per unit
          center <- mean(u_values)
          
          # Assume constant subgroup size n
          n <- 5
          
          # Calculate the standard deviation for the u chart
          sigma_u <- sqrt(center / n)
          
          # Calculate control limits
          ucl <- center + 3 * sigma_u
          lcl <- center - 3 * sigma_u
          lcl <- ifelse(lcl < 0, 0, lcl)  # Ensure LCL is not negative
          
          
          p <- plot_ly(data = data.frame(Sample = seq_along(u_values), Measurement = u_values),
                       x = ~Sample, y = ~Measurement, type = 'scatter', mode = 'lines+markers', name = 'u') %>%
            add_trace(y = rep(ucl, length(u_values)), mode = 'lines', name = 'UCL', 
                      line = list(dash = 'dot', color = 'red')) %>%
            add_trace(y = rep(lcl, length(u_values)), mode = 'lines', name = 'LCL', 
                      line = list(dash = 'dot', color = 'green')) %>%
            add_trace(y = rep(center, length(u_values)), mode = 'lines', name = 'Center Line', 
                      line = list(dash = 'dash', color = 'blue')) %>%
            layout(title = paste(chart_type, input$column3),
                   xaxis = list(title = "Subgroup"),
                   yaxis = list(title = "Defects per Unit"),
                   legend = list(x = 0.1, y = 0.9)) %>%
            config(toImageButtonOptions = list(
              format = "png",
              filename = paste0("u_Chart_", format(Sys.time(), "%Y%m%d_%H%M%S"))
            ))
        
        }else if (chart_type == "Grouped Bar Chart" ) {
          # Common UI for Grouped 
          req(input$xcol, input$ycol, input$fillcol)  # Ensure all necessary inputs are selected
          
          if (!is.character(stored_result()[[input$xcol]]) && !is.factor(stored_result()[[input$xcol]])) {
            showNotification(HTML("<b>For Grouped Bar Chart, X-axis must be categorical.</b>"), type = "error")
            return()
          }
          if (!is.numeric(stored_result()[[input$ycol]])) {
            showNotification(HTML("<b>For Grouped Bar Chart, Y-axis must be numeric.</b>"), type = "error")
            return()
          }
          if (!is.character(stored_result()[[input$fillcol]]) && !is.factor(stored_result()[[input$fillcol]])) {
            showNotification(HTML("<b>For Grouped Bar Chart, Fill column must be categorical.</b>"), type = "error")
            return()
          }
          
          # Create the plot for Grouped or Stacked Bar Chart
          p <- plot_ly(data = stored_result(), x = ~get(input$xcol), y = ~get(input$ycol), type = 'bar', name = ~get(input$fillcol), 
                       color = ~get(input$fillcol), 
                       text = ~paste("Category:", get(input$xcol), "<br>Subcategory:", get(input$fillcol), "<br>Value:", get(input$ycol)),
                       hoverinfo = "text") %>%
            layout(barmode = 'group',
                   title = paste("Grouped Bar Chart"), 
                   xaxis = list(title = input$xcol), yaxis = list(title = input$ycol),
                   legend = list(title = list(text = paste(input$fillcol))))
          
          
        } else if(input$chart_type == "Stacked Bar Chart"){
          req(input$xcol1, input$ycol1, input$fillcol1)  # Ensure all necessary inputs are selected
          
          if (!is.character(stored_result()[[input$xcol1]]) && !is.factor(stored_result()[[input$xcol1]])) {
            showNotification(HTML("<b>For Stacked Bar Chart, X-axis must be categorical.</b>"), type = "error")
            return()
          }
          if (!is.numeric(stored_result()[[input$ycol1]])) {
            showNotification(HTML("<b>For Stacked Bar Chart, Y-axis must be numeric.</b>"), type = "error")
            return()
          }
          if (!is.character(stored_result()[[input$fillcol1]]) && !is.factor(stored_result()[[input$fillcol1]])) {
            showNotification(HTML("<b>For Stacked Bar Chart, Fill column must be categorical.</b>"), type = "error")
            return()
          }
          
          p <- plot_ly(data = stored_result(), x = ~get(input$xcol1), y = ~get(input$ycol1), type = 'bar', name = ~get(input$fillcol1), 
                       color = ~get(input$fillcol1), 
                       text = ~paste("Category:", get(input$xcol1), "<br>Subcategory:", get(input$fillcol1), "<br>Value:", get(input$ycol1)),
                       hoverinfo = "text") %>%
            layout(barmode = 'stack',
                   title = paste("Stacked Bar Chart"), 
                   xaxis = list(title = input$xcol1), yaxis = list(input$ycol1),
                   legend = list(title = list(text = paste(input$fillcol1))))
          
          
        } else if (chart_type == "Stretched Bar Chart") {
          req(input$xcol2, input$ycol2)  # Ensure necessary inputs are selected
          
          if (!is.character(stored_result()[[input$xcol2]]) && !is.factor(stored_result()[[input$xcol2]])) {
            showNotification(HTML("<b>For Stretched Bar Chart, X-axis must be categorical.</b>"), type = "error")
            return()
          }
          if (!is.numeric(stored_result()[[input$ycol2]])) {
            showNotification(HTML("<b>For Stretched Bar Chart, Y-axis must be numeric.</b>"), type = "error")
            return()
          }
          
          p <- plot_ly(data = stored_result(), x = ~get(input$xcol2), y = ~get(input$ycol2), type = 'bar',
                       marker = list(color = 'steelblue')) %>%
            layout(
              title = "Stretched Bar Chart",
              xaxis = list(title = input$xcol2),
              yaxis = list(title = input$ycol2)
            )
          
        } else if (chart_type == "HeatMap") {
          req(input$rowcol, input$colcol, input$valuecol)  # Ensure necessary inputs are selected
          
          if (!is.character(stored_result()[[input$rowcol]]) && !is.factor(stored_result()[[input$rowcol]])) {
            showNotification(HTML("<b>For Heatmap, Row column must be categorical.</b>"), type = "error")
            return()
          }
          if (!is.character(stored_result()[[input$colcol]]) && !is.factor(stored_result()[[input$colcol]])) {
            showNotification(HTML("<b>For Heatmap, Column column must be categorical.</b>"), type = "error")
            return()
          }
          if (!is.numeric(stored_result()[[input$valuecol]])) {
            showNotification(HTML("<b>For Heatmap, Value column must be numeric.</b>"), type = "error")
            return();
          }
          
          heat_data <- dcast(stored_result(), formula = paste(input$rowcol, "~", input$colcol), value.var = input$valuecol,fun.aggregate = sum)
          matrix_data <- as.matrix(heat_data[, -1])
          
          p <- plot_ly(
            z = matrix_data,
            x = colnames(heat_data)[-1],
            y = heat_data[[input$rowcol]],
            type = 'heatmap',
            colorscale = 'Viridis'
          )
        }
        
        # Store the plot
        generated_plot(p)
        output$control_chart <- renderPlotly({ p })  # Render the plot
      })
      
      output$download_chart <- downloadHandler(
        filename = function() {
          paste("Control_Chart_", Sys.time(), ".pdf", sep = "")
        },
        content = function(file) {
          # Generate the Plotly plot
          plot_to_print <- generated_plot()
          
          if (!is.null(plot_to_print)) {
            # Save the Plotly plot as a PNG image
            temp_image <- tempfile(fileext = ".png")
            plotly::export(plot_to_print, file = temp_image)
            
            # Open the PDF device
            pdf(file, height = 8.0, width = 11.0)
            
            # Insert the saved PNG image into the PDF
            img <- grid::rasterGrob(png::readPNG(temp_image), width = unit(1, "npc"), height = unit(1, "npc"))
            gridExtra::grid.arrange(img)
            
            # Close the PDF device
            dev.off()
          } else {
            # If no plot available, show "No plot available"
            pdf(file, height = 8.0, width = 11.0)
            grid::grid.text("No plot available.", x = 0.5, y = 0.5, gp = grid::gpar(fontsize = 12))
            dev.off()
          }
        }
      )
      
    }) # plot
  }# statistics observeEvent
  
  
  #______________________________All report____________________________________ 
  observeEvent(input$alldata, {
    shinyjs::show("allresults")
    
    
    output$history_table <- renderDT({
      datatable(results$history, escape = FALSE, rownames = FALSE)
    })
    
    # Handle view action
    observeEvent(input$view, {
      selected_row <- as.numeric(input$view)
      if (selected_row <= nrow(results$history)) {
        test_name <- results$history[selected_row, "TestName"]
        result_data <- NULL
        plot_data <- NULL
        explained_variance_data <- NULL
        # Define variables based on test type
        if (test_name == "One-Sample T-Test") {
          result_data <- results$t_res_table1
          ttest1_variable <- input$ttest_column1
        } else if (test_name == "Two-Sample T-Test") {
          result_data <- results$t_res_table2
          ttest2_variable1 <- input$ttest_column2
          ttest2_variable2 <- input$ttest_column3
        } else if (test_name == "One-Way ANOVA") {
          result_data <- results$aov_res_table1
          anova1_variable <- input$oneway_dv
          anova1_factor <- input$oneway_factor
        } else if (test_name == "Two-Way ANOVA") {
          result_data <- results$aov_res_table2
          anova2_variable <- input$twoway_dv
          anova2_factor1 <- input$twoway_factor1
          anova2_factor2 <- input$twoway_factor2
        } else if (test_name == "One-Way MANOVA") {
          result_data <- results$manova_res_table1
          manova1_dv <- input$manova_oneway_dv
          manova1_factor <- input$manova_oneway_factor
        } else if (test_name == "Two-Way MANOVA") {
          result_data <- results$manova_res_table2
          manova2_dv <- input$manova_twoway_dv
          manova2_factor1 <- input$manova_twoway_factor1
          manova2_factor2 <- input$manova_twoway_factor2
        } else if (test_name == "Repeated Measures ANOVA") {
          result_data <- results$rep_anova
          rep_column1 <- input$rep_column
          rep_column2 <- input$subject_column
          rep_column3 <- input$time_column
        } else if (test_name == "Principal Component Analysis (PCA)") {
          result_data <- results$pca_components
          explained_variance_data <- results$pca_explained_variance
          plot_data <- results$pca_plot_static
          pca_column1 <- input$pca_column1
          pca_column2 <- input$pca_column2
          pca_interpretation_text <- results$pca_interpretation_text
        }
        
        # Create the download handler
        output$downloadPDF <- downloadHandler(
          filename = function() {
            filename <- paste0(gsub(" ", "_", test_name), "_results_", format(Sys.Date(), "%Y-%m-%d"), "-", format(Sys.time(), "%H-%M-%S"), ".pdf")
          },
          content = function(file) {
            rmd_content <- ""
            # Generate R Markdown content based on test type
            if (test_name == "One-Sample T-Test") {
              rmd_content <- glue('
              **One-Sample T-Test Summary**\n\n
              To compare the mean of a single sample to a known value, a one-sample T-test is used.\n\n
              **The following are the results:**\n\n
              **Variable Tested:** {ttest1_variable}\n\n
              **P-Value:** {result_data$P_Value}\n\n
              **Confidence Interval:** {result_data$Confidence_Interval}\n\n
              **Interpretation:** A p-value less than 0.05 indicates that the sample mean is significantly different from the hypothesized mean.\n\n
              **One-Sample T-Test Results Table**
              ```{{r, echo=FALSE}}
              knitr::kable(result_data)
              ```
            ')
            } else if (test_name == "Two-Sample T-Test") {
              rmd_content <- glue('
              **Two-Sample T-Test Summary**\n\n
              To compare the means of two independent samples, a two-sample T-test is used.\n\n
              **The following are the results:**\n\n
              **First Numric Variable:**: {ttest2_variable1}\n\n
              **Second Numric Variable:**: {ttest2_variable2}\n\n
              **Interpretation:** A p-value less than 0.05 suggests that there is a significant difference between the two sample means.\n\n
              **Two-Sample T-Test Results Table**\n\n
              ```{{r, echo=FALSE}}
              knitr::kable(result_data)
              ```
            ')
            } else if (test_name == "One-Way ANOVA") {
              rmd_content <- glue('
              **One-Way ANOVA Summary**\n\n
              ANOVA (Analysis of Variance) is used to compare means across multiple groups.\n\n
              **The following are the results:**\n\n
              **Dependent Variable**: {anova1_variable}\n\n
              **Independent Variable**: {anova1_factor}\n\n
              **Interpretation:** A significant p-value indicates that at least one group mean is different from the others.\n\n
              **One-Way ANOVA Results Table**\n\n
              ```{{r, echo=FALSE}}
              knitr::kable(result_data)
              ```
            ')
            } else if (test_name == "Two-Way ANOVA") {
              rmd_content <- glue('
              **Two-Way ANOVA Summary**\n\n
              Two-Way ANOVA is used to examine the interaction between two factors.\n\n
              **The following are the results:**\n\n
              **Dependent Variable**: {anova2_variable}\n\n
              **Independent Variable 1**: {anova2_factor1}\n\n
              **Independent Variable 2**: {anova2_factor2}\n\n
              **Interpretation:** A significant p-value indicates that at least one group mean is different from the others, considering the interaction between factors.\n\n
              **Two-Way ANOVA Results Table**\n\n
              ```{{r, echo=FALSE}}
              knitr::kable(result_data)
              ```
            ')
            } else if (test_name == "One-Way MANOVA") {
              rmd_content <- glue('
              **One-Way MANOVA Summary**\n\n
              One-Way MANOVA is used to determine whether multiple dependent variables are affected by a single categorical factor.\n\n
             
              **Interpretation:** A significant p-value suggests that at least one of the dependent variables is significantly affected by the factor.\n\n
              **One-Way MANOVA Results Table**\n\n
              ```{{r, echo=FALSE}}
              knitr::kable(result_data)
              ```
            ')
            } else if (test_name == "Two-Way MANOVA") {
              rmd_content <- glue('
              **Two-Way MANOVA Summary**\n\n
              Two-Way MANOVA is used to determine whether multiple dependent variables are influenced by two categorical factors and their interaction.\n\n
              **Interpretation:** A significant p-value suggests that at least one dependent variable is significantly affected by the two factors or their interaction.\n\n
              **Two-Way MANOVA Results Table**\n\n
              ```{{r, echo=FALSE}}
              knitr::kable(result_data)
              ```
            ')
            } else if (test_name == "Repeated Measures ANOVA") {
              rmd_content <- glue('
              **Repeated Measures ANOVA Summary**\n\n
              Repeated Measures ANOVA is used to determine whether there are any statistically significant differences between the means of three or more related groups.\n\n
              **The following are the results:**\n\n
              **Dependent Variable:** {rep_column1}\n\n
              **Subject Identifier:** {rep_column2}\n\n
              **Time/Condition:** {rep_column3}\n\n
              **Interpretation:** A significant p-value indicates that there is a significant difference between the means of the related groups.\n\n
              **Repeated Measures ANOVA Results Table**\n\n
              ```{{r, echo=FALSE}}
              knitr::kable(result_data)
              ```
            ')
            } else if (test_name == "Principal Component Analysis (PCA)") {
              rmd_content <- glue('
              **PCA Summary**\n\n
              Principal Component Analysis (PCA) is a technique used to reduce the dimensionality of data while preserving as much variance as possible.\n\n
              **The following are the results:**\n\n
              **Variables Analyzed**: {pca_column1} and {pca_column2}\n\n
              **Interpretation:** PCA results can help identify patterns in the data and visualize the relationships between variables.\n\n
              **Principal Components Coordinates**\n\n
              ```{{r, echo=FALSE}}
              knitr::kable(result_data)
              ```
              **Explained Variance Table**\n\n
              ```{{r, echo=FALSE}}
              knitr::kable(explained_variance_data)
              ```
              **Interpretation:** {pca_interpretation_text}\n\n
              **PCA Plot**\n\n
              ```{{r, echo=FALSE, fig.cap="PCA Plot"}}
              print(plot_data)
              ```
            ')
            }
            # Create and render the PDF
            print(rmd_content)
            # Create and render the PDF
            tmp_rmd <- tempfile(fileext = ".Rmd")
            writeLines(c(
              "---",
              "title: \"Statistical Analysis Results\"",
              "output: pdf_document",
              "---",
              "",
              rmd_content
            ), tmp_rmd)
            rmarkdown::render(tmp_rmd, output_file = file,output_format = "pdf_document", quiet = TRUE)
            unlink(tmp_rmd)
          }
        )
        # Modal dialog and output renders remain the same
        showModal(modalDialog(
          title = glue("Test Result for {test_name}"),
          renderUI({
            if (!is.null(result_data)) {
              if (test_name == "Principal Component Analysis (PCA)") {
                tagList(
                  DTOutput("view_result"),
                  DTOutput("view_explained_variance"),
                  plotOutput("view_plot"),
                  uiOutput("pca_interpretation")
                )
              } else if (test_name == "Meta Analysis") {
                tagList(
                  DTOutput("view_result"),
                  plotOutput("forest_plot"),  # Render forest plot
                  plotOutput("funnel_plot")   # Render funnel plot
                )
              }else {
                tagList(
                  DTOutput("view_result"),
                  if (!is.null(plot_data)) {
                    plotOutput("view_plot")
                  }
                )
              }
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
        output$view_result <- renderDT({
          datatable(result_data, extensions = 'Buttons',
                    options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE))
        })
        if (!is.null(explained_variance_data)) {
          output$view_explained_variance <- renderDT({
            datatable(explained_variance_data, extensions = 'Buttons',
                      options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE))
          })
        }
        output$view_plot <- renderPlot({
          if (!is.null(plot_data)) {
            print(plot_data)
          }
        })
      }
    })
    # Handle delete action
    observeEvent(input$delete, {
      selected_row <- as.numeric(input$delete)
      if (selected_row <= nrow(results$history)) {
        results$history <- results$history[-selected_row, ]
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
          "  ttest1: NULL",
          "  ttest2: NULL",
          "  Interpretationttest: NULL ",
          "  anova1: NULL",
          "  anova2: NULL",
          "  pca_components: NULL",
          "  pca_explained_variance: NULL",
          "  pca_plot: NULL",
          "  ttest1_variable: NULL",
          "  ttest2_variable: NULL",
          "  anova1_variable: NULL",
          "  anova1_factor: NULL",
          "  anova2_variable: NULL",
          "  anova2_factor1: NULL",
          "  anova2_factor2: NULL",
          "  manova1: NULL",
          "  manova1_dv: NULL",
          "  manova1_factor: NULL",
          "  manova2: NULL",
          "  manova2_dv: NULL",
          "  manova2_factor1: NULL",
          "  manova2_factor2: NULL",
          "  rep_result : NULL",
          "  rep_column1 : NULL",
          "  rep_column2 : NULL",
          "  rep_column3 : NULL",
          "  pca_column1: NULL",
          "  pca_column2: NULL",
          "  pca_interpretation_text: NULL",
          "---",
          "",
          "# Results",
          ""
        )
        
        ordered_tests <- c("One-Sample T-Test", "Two-Sample T-Test", "One-Way ANOVA", "Two-Way ANOVA",
                           "One-Way MANOVA", "Two-Way MANOVA", "Repeated Measures ANOVA", "Principal Component Analysis (PCA)")
        
        for (test_name in ordered_tests) {
          if (!(test_name %in% results$history$TestName)) next
          
          if (test_name == "One-Sample T-Test" && !is.null(results$t_res_table1)) {
            report_lines <- c(report_lines,
                              "## One-Sample T-Test Summary",
                              "To compare the mean of a single sample to a known value, a one-sample T-test is used.",
                              "",
                              "The following are the results:",
                              "",
                              "**Variable Tested:** `r params$ttest1_variable`.",
                              "",
                              "**P-Value:** `r params$ttest1$P_Value`.",
                              "",
                              "**Confidence Interval:** `r params$ttest1$Confidence_Interval`.",
                              "",
                              "**Interpretation:** A p-value less than 0.05 indicates that the sample mean is significantly different from the hypothesized mean.",
                              "",
                              "### One-Sample T-Test Results",
                              "```{r, echo=FALSE}",
                              "if (!is.null(params$ttest1)) {",
                              "  knitr::kable(params$ttest1, format='latex', booktabs=TRUE, align = 'c') %>%",
                              "    kableExtra::kable_styling(position = 'center', full_width = FALSE, latex_options = c('hold_position', 'center'))",
                              "} else {",
                              "  cat('No results for One-sample T-Test.')",
                              "}",
                              "```",
                              ""
                             
                              
                              
            )
          }
          
          if (test_name == "Two-Sample T-Test" && !is.null(results$t_res_table2)) {
            report_lines <- c(report_lines,
                              "## Two-Sample T-Test Summary",
                              "To compare the means of two independent samples, a two-sample T-test is used.",
                              "",
                              "The following are the results:",
                              "",
                              "**Variables Tested**: `r params$ttest2_variable`",
                              "",
                              "**P-Value**: `r params$ttest2$P_Value`",
                              "",
                              "**Confidence Interval**: `r params$ttest2$Confidence_Interval`",
                              "",
                              "**Interpretation**: A p-value less than 0.05 suggests that there is a significant difference between the two sample means.",
                              "",
                              "### Two-Sample T-Test Results",
                              "```{r, echo=FALSE}",
                              "if (!is.null(params$ttest2)) {",
                              "  knitr::kable(params$ttest2, format='latex', booktabs=TRUE, align = 'c') %>%",
                              "    kableExtra::kable_styling(position = 'center', full_width = FALSE, latex_options = c('hold_position', 'center'))",
                              "} else {",
                              "  cat('No results for Two-sample T-Test.')",
                              "}",
                              "```",
                              ""
                             
            )
          }
          
          if (test_name == "One-Way ANOVA" && !is.null(results$aov_res_table1)) {
            report_lines <- c(report_lines,
                              "## One-Way ANOVA Summary",
                              "ANOVA (Analysis of Variance) is used to compare means across multiple groups.",
                              "",
                              "The following are the results:",
                              "",
                              "**Dependent Variable**: `r params$anova1_variable`",
                              "",
                              "**Independent Variable**: `r params$anova1_factor`",
                              "",
                              "**Interpretation**: A significant p-value indicates that at least one group mean is different from the others.",
                              "",
                              "### One-Way ANOVA Results",
                              "```{r, echo=FALSE}",
                              "if (!is.null(params$anova1)) {",
                              "  knitr::kable(params$anova1, format='latex', booktabs=TRUE, align = 'c') %>%",
                              "    kableExtra::kable_styling(position = 'center', full_width = FALSE, latex_options = c('hold_position', 'center'))",
                              "} else {",
                              "  cat('No results for One-way ANOVA.')",
                              "}",
                              "```",
                              ""
            )
          }
          
          
          if (test_name == "Two-Way ANOVA" && !is.null(results$aov_res_table2)) {
            report_lines <- c(report_lines,
                              "## Two-Way ANOVA Summary",
                              "Two-Way ANOVA is used to examine the interaction between two factors.",
                              "",
                              "The following are the results:",
                              "",
                              "**Dependent Variable**: `r params$anova2_variable`",
                              "",
                              "**Independent Variable**: `r params$anova2_factor1`",
                              "",
                              "**Independent Variable**: `r params$anova2_factor2`",
                              "",
                              "**Interpretation**: A significant p-value indicates that at least one group mean is different from the others, considering the interaction between factors.",
                              "",
                              "### Two-Way ANOVA Results",
                              "```{r, echo=FALSE}",
                              "if (!is.null(params$anova2)) {",
                              "  knitr::kable(params$anova2, format='latex', booktabs=TRUE, align = 'c') %>%",
                              "    kableExtra::kable_styling(position = 'center', full_width = FALSE, latex_options = c('hold_position', 'center'))",
                              "} else {",
                              "  cat('No results for Two-way ANOVA.')",
                              "}",
                              "```",
                              ""
            )
          }
          
          if (test_name == "One-Way MANOVA" && !is.null(results$manova_res_table1)) {
            report_lines <- c(report_lines,
                              "## One-Way MANOVA Summary",
                              "One-Way MANOVA is used to determine whether multiple dependent variables are affected by a single categorical factor.",
                              "",
                              "**Dependent Variables:** `r params$manova1_dv`",
                              "",
                              "** Independent Variable:** `r params$manova1_factor`",
                              "",
                              "### One-Way MANOVA Results",
                              "```{r, echo=FALSE}",
                              "if (!is.null(params$manova1)) {",
                              "  knitr::kable(params$manova1, format='latex', booktabs=TRUE, align = 'c') %>%",
                              "    kableExtra::kable_styling(position = 'center', full_width = FALSE, latex_options = c('hold_position', 'center'))",
                              "} else {",
                              "  cat('No results for One-Way MANOVA.')",
                              "}",
                              "```",
                              ""
            )
          }
          
          if (test_name == "Two-Way MANOVA" && !is.null(results$manova_res_table2)) {
            report_lines <- c(report_lines,
                              "## Two-Way MANOVA Summary",
                              "Two-Way MANOVA is used to determine whether multiple dependent variables are influenced by two categorical factors and their interaction.",
                              "",
                              "**Dependent Variables:** `r params$manova2_dv`",
                              "",
                              "** Independent Variable 1:** `r params$manova2_factor1`",
                              "",
                              "** Independent Variable 2:** `r params$manova2_factor2`",
                              "",
                              "### Two-Way MANOVA Results",
                              "```{r, echo=FALSE}",
                              "if (!is.null(params$manova2)) {",
                              "  knitr::kable(params$manova2, format='latex', booktabs=TRUE, align = 'c') %>%",
                              "    kableExtra::kable_styling(position = 'center', full_width = FALSE, latex_options = c('hold_position', 'center'))",
                              "} else {",
                              "  cat('No results for Two-Way MANOVA.')",
                              "}",
                              "```",
                              ""
            )
          }
          
          if (test_name == "Repeated Measures ANOVA" && !is.null(results$rep_anova)) {
            report_lines <- c(report_lines,
                              "## Repeated Measures ANOVA Summary",
                              "Repeated Measures ANOVA is used to compare means across multiple measurements taken on the same subjects.",
                              "",
                              "**Dependent Variable**: `r params$rep_column1 `",
                              "",
                              "** Independent Variable 1**: `r params$rep_column2 `",
                              "",
                              "** Independent Variable 2**: `r params$rep_column3 `",
                              "",
                              "### Repeated Measures ANOVA Results",
                              "```{r, echo=FALSE}",
                              "if (!is.null(params$rep_result)) {",
                              "  knitr::kable(params$rep_result, format='latex', booktabs=TRUE, align = 'c') %>%",
                              "    kableExtra::kable_styling(position = 'center', full_width = FALSE, latex_options = c('hold_position', 'center'))",
                              "} else {",
                              "  cat('No results for Repeated Measures ANOVA.')",
                              "}",
                              "```",
                              ""
            )
          }
          
          if (test_name == "Principal Component Analysis (PCA)" && !is.null(results$pca_components)) {
            report_lines <- c(report_lines,
                              "## PCA Summary",
                              "Principal Component Analysis (PCA) is a technique used to reduce the dimensionality of data while preserving as much variance as possible.",
                              "",
                              "**Variables Analyzed**: `r params$pca_column1` and `r params$pca_column2`",
                              "",
                              "### Principal Components Coordinates",
                              "```{r, echo=FALSE}",
                              "if (!is.null(params$pca_components)) {",
                              "  knitr::kable(params$pca_components, format='latex', booktabs=TRUE, align = 'c') %>%",
                              "    kableExtra::kable_styling(position = 'center', full_width = FALSE, latex_options = c('hold_position', 'center'))",
                              "} else {",
                              "  cat('No PCA performed.')",
                              "}",
                              "```",
                              "",
                              "### PCA Explained Variance",
                              "```{r, echo=FALSE}",
                              "if (!is.null(params$pca_explained_variance)) {",
                              "  knitr::kable(params$pca_explained_variance, format='latex', booktabs=TRUE, align = 'c') %>%",
                              "    kableExtra::kable_styling(position = 'center', full_width = FALSE, latex_options = c('hold_position', 'center'))",
                              "} else {",
                              "  cat('No results for PCA Explained Variance.')",
                              "}",
                              "```",
                              "",
                              "### PCA Biplot",
                              "```{r, echo=FALSE, fig.height=6, fig.width=8, fig.pos='H'}",
                              "if (!is.null(params$pca_plot)) {",
                              "  print(params$pca_plot)",
                              "} else {",
                              "  cat('No PCA plot available.')",
                              "}",
                              "```",
                              "",
                              "### PCA Interpretation",
                              "`r params$pca_interpretation_text`",
                              "```",
                              ""
            )
          }
        }
        
        writeLines(report_lines, con = tempReport)
        
        rmarkdown::render(tempReport,
                          output_file = file,
                          output_format = "pdf_document",
                          params = list(
                            ttest1 = results$t_res_table1,
                            ttest2 = results$t_res_table2,
                            Interpretationttest = results$t_test_interpretation_html,
                            anova1 = results$aov_res_table1,
                            anova1_variable = input$oneway_dv,
                            anova1_factor = input$oneway_factor,
                            anova2 = results$aov_res_table2,
                            anova2_variable = input$twoway_dv,
                            anova2_factor1 = input$twoway_factor1,
                            anova2_factor2 = input$twoway_factor2,
                            manova1 = results$manova_res_table1,
                            manova1_dv = input$manova_oneway_dv,
                            manova1_factor = input$manova_oneway_factor,
                            manova2 = results$manova_res_table2,
                            manova2_dv = input$manova_twoway_dv,
                            manova2_factor1 = input$manova_twoway_factor1,
                            manova2_factor2 = input$manova_twoway_factor2,
                            rep_result = results$rep_anova,
                            rep_column1 = input$rep_column,
                            rep_column2 = input$subject_column,
                            rep_column3 = input$time_column,
                            pca_components = results$pca_components,
                            pca_explained_variance = results$pca_explained_variance,
                            pca_plot = results$pca_plot_static,
                            ttest1_variable = input$ttest_column1,
                            ttest2_variable = paste(input$ttest_column2, "and", input$ttest_column3),
                            pca_column1 = input$pca_column1,
                            pca_column2 = input$pca_column2,
                            pca_interpretation_text = results$pca_interpretation_text
                          ),
                          quiet = TRUE
        )
      }
    )
    
  })
}

shinyApp(ui, server)


