##################################################
# Interactive functions to train a FlowSOM model #
##################################################


##### IMPORTS #####

library(flowCore)
library(FlowSOM)
library(bsicons)
library(shinyjs)


##### UI #####

flowsom_train_ui <- function(id, panel_type, marker_options, marker_selected) {
  ns <- NS(id)
  
  # === Marker options === #
  #Retrieve the list of marker choices
  choices <- marker_options[[panel_type]]
  selected <- marker_selected[[panel_type]]
  
  # Split into two columns
  n <- length(choices)
  half <- ceiling(n / 2)
  col1 <- choices[1:half]
  col2 <- choices[(half + 1):n]
  
  tagList(
    h3("Model training"),
    h4("FlowSOM"),
    p("FlowSOM uses an unsupersived machine learning technique called self-organizing maps (SOM) 
      which performs clustering and dimensionality reduction. FlowSOM will identify similar cells and place 
      them in the same cluster (clustering) and allow visualiszation of these clusters via a minimum 
      spanning tree (MST). Additionally, a higher-level clustering is performed to categorize similar 
      clusters together (meta-clusters)."),
    p("Adjust training parameters and launch model training. Only one model can be saved and analyzed at a time.
      Training a new model will delete the previous one."),
    
    numericInput(ns("n_cells"), 
                 label = tagList(
                   "Max Cells to Aggregate",
                   tooltip(bs_icon("question-circle-fill"), "Maximum number of cells to use from all FCS files combined.", placement="right")),
                 value = 500000, min = 10000, step = 50000),
    
    h6(tagList(
        "Clustering markers",
        tooltip(bs_icon("question-circle-fill"),"Markers that will be used to cluster cells.", placement="right"))),
    fluidRow(
      column(
        6,
        checkboxGroupInput(ns("cluster_markers_col1"), label = NULL, choices = col1,
                           selected = selected[selected %in% col1])
      ),
      column(
        6,
        checkboxGroupInput(ns("cluster_markers_col2"), label = NULL, choices = col2,
                           selected = selected[selected %in% col2])
      )
    ),
    
    numericInput(ns("xdim"),
                 label = tagList(
                   "SOM Grid X",
                   tooltip(bs_icon("question-circle-fill"),"Number of nodes horizontally on the SOM grid. Total clusters = X × Y.")),
                 value = 10, min = 2),
    
    numericInput(ns("ydim"),
                 label = tagList(
                   "SOM Grid Y",
                   tooltip(bs_icon("question-circle-fill"),"Number of nodes vertically on the SOM grid. Total clusters = X × Y.")),
                 value = 10, min = 2),
    
    numericInput(ns("n_meta"),
                 label = tagList(
                   "Number of Metaclusters",
                   tooltip(bs_icon("question-circle-fill"), "Number of cell groups to merge similar clusters.")),
                 value = 12, min = 2),
    
    actionButton(ns("train_btn"),
                 label = tagList(
                  "Start training",
                  tooltip(tags$span(bs_icon("question-circle-fill"), style = "color: red;"), "Only one model can be saved at a time. Retraining a new model will delete the previous one.")), 
                  value = FALSE),
    
    tags$hr(),
    tags$h5("Training Log"),
    verbatimTextOutput(ns("training_log")),
    plotOutput(ns("fsom_tree_plot"))
  )
}




##### SERVER #####

flowsom_train_server <- function(id, fcs_path, marker_options, marker_selected, panel, results_dir, static_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # === Paths === #
    model_path <- file.path(results_dir, paste0("fsom_model_", panel, ".rds"))
    aggregate_path <- file.path(results_dir, paste0("aggregate_", panel, ".fcs"))
    summary_path = file.path(static_dir, paste0("summary_", panel, ".pdf"))
    
    fsom_model <- reactiveVal(NULL)
    log_text <- reactiveVal("")
    
    # === Start training === #
    observeEvent(input$train_btn, {
      shinyjs::disable("train_btn")  # Disable button at start
      shinyjs::html(id = "training_log", "")  # Clear log
      
      tryCatch({
        # -- Input parameters -- #
        n_cells <- input$n_cells
        xdim <- input$xdim
        ydim <- input$ydim
        n_meta <- input$n_meta
        cluster_markers <- c(input$cluster_markers_col1, input$cluster_markers_col2)
        
        # -- Delete old model and training steps -- #
        withCallingHandlers({
          message("Training FlowSOM model... this might take a while.")
          
          if (file.exists(model_path)) {
            file.remove(model_path)
            message(paste(">> Deleted old model:", model_path))
          }
          if (file.exists(aggregate_path)) {
            file.remove(aggregate_path)
          }
          if (file.exists(summary_path)) {
            file.remove(summary_path)
          }
          
          message(">> Loading FCS files...")
          fcs_norm <- flowCore::read.flowSet(
            path = fcs_path,
            pattern = "\\.fcs$",
            transformation = FALSE,
            truncate_max_range = FALSE
          )
          
          message(">> Aggregating cells...")
          agg <- FlowSOM::AggregateFlowFrames(fcs_norm, cTotal = n_cells, writeOutput = FALSE)
          
          message(">> Training FlowSOM model...")
          fsom <- FlowSOM::FlowSOM(
            input = agg,
            compensate = FALSE,
            transform = FALSE,
            scale = FALSE,
            colsToUse = cluster_markers,
            xdim = xdim,
            ydim = ydim,
            nClus = n_meta
          )
          
          message(">> Saving model...")
          dir.create(dirname(model_path), showWarnings = FALSE, recursive = TRUE)
          saveRDS(fsom, model_path)
          
          message(">> Saving model summary...")
          FlowSOM::FlowSOMmary(fsom = fsom, plotFile = summary_path)
          
          message("TRAINING COMPLETE. Visit other FlowSOM tabs to explore the model.")
        },
        # -- Custom messages -- #
        message = function(m) {
          timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          shinyjs::html(id = "training_log", html = paste0(timestamp, " ", m$message, "\n"), add = TRUE)
          invokeRestart("muffleMessage")
        })
      },
      error = function(e) {
        showNotification(paste("Training error:", conditionMessage(e)), type = "error")
      },
      finally = {
        shinyjs::enable("train_btn")  # re-enable button
      })
    })
  })
}
