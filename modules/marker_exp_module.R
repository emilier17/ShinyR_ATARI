################################################################################
# Functions to see single marker expression among all clusters in FlowSOM tree #
################################################################################


##### IMPORTS #####

library(FlowSOM)
library(ggplot2)
library(ggpubr)


##### UI #####

marker_exp_ui <- function(id, all_markers, panel) {
  ns <- NS(id)
  
  # === Marker options per panel === #
  markers_options <- all_markers[[panel]]
  
  # === UI setup === #
  tagList(
    h3("Cluster Marker Expression"),
    h4("FlowSOM"),
    p("Visualize the expression levels of a singular marker in all clusters."),
    
    uiOutput(ns("qc_warning")),  # if no model exists yet
  
    selectInput(inputId = ns("marker_input"), "Select a marker", choices=markers_options),
    
    actionButton(ns("run_view"), "View Expression"), 
    
    uiOutput(ns("marker_exp_pdf")))
}


##### SERVER #####

marker_exp_server <- function(id, panel, all_markers, fsom_model, static_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cat("Marker expression plotting server started", id, "\n")
    
    # === Warning if no model === #
    output$qc_warning <- renderUI({
      req(is.null(fsom_model()))
      tags$div(style = "color: red; font-weight: bold;", "No model found. Visit the 'Training' tab to create one first.")
    })
    
    # === Start analysis === #
    observeEvent(input$run_view, {
      req(input$marker_input) 
      marker <- input$marker_input
      
      shinyjs::disable("run_view") # disable button while analysis is running
      
      # -- Create and save the plot -- #
      tryCatch({
        PlotMarker(fsom = fsom_model()$model, marker = marker)
        marker_exp_path <- file.path(static_dir, paste0("marker_exp_", panel, ".pdf"))
        ggsave(marker_exp_path)
        
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)), type = "error")
        }, finally = {
          shinyjs::enable("run_view")  # re-enable button after success or error
        })
      })
    
    
    # == Display results == #
    
    # -- Reactive file reader -- #
    marker_mod_time <- reactiveFileReader(
      intervalMillis = 2000,
      session = session,
      filePath = file.path("www", "flowsom", paste0("marker_exp_", panel, ".pdf")),
      readFunc = function(path) file.info(path)$mtime)
    
    # -- PDF display -- #
    output$marker_exp_pdf <- renderUI({
      time <- marker_mod_time()  # reactive dependency
      
      filePath = file.path("www", "flowsom", paste0("marker_exp_", panel, ".pdf"))
      if (!file.exists(filePath)) {
        return(tags$div(style = "color: gray; font-style: italic;",
                        "Please generate a marker expression plot first."))
      }
      
      rel_web_path <- file.path("flowsom", paste0("marker_exp_", panel, ".pdf"))
      
      tags$iframe(
        src = sprintf("%s?time=%s", rel_web_path, as.numeric(time)),  # bust cache
        style = "width:100%; height:800px; border:none;"
      )
    })
  
  })
}

