###################################################################
# Functions to find specific cell populations within FlowSOM tree #
###################################################################


##### IMPORTS #####

library(FlowSOM)
library(ggplot2)
library(ggpubr)



##### UI #####

pop_search_ui <- function(id, all_markers) {
  ns <- NS(id)
  
  tagList(
    h3("Cellular Population Search"),
    h4("FlowSOM"),
    p("Find clusters with specific marker expression patterns that are related to distinct cellular populations.
      As of FlowSOM v.2.14.0, the options for marker expression are either 'low' or 'high'."),
    
    uiOutput(ns("qc_warning")),  # if no model exists yet
    
    uiOutput(ns("query_list_ui")),

    div(style = "display: flex; justify-content: flex-end; gap: 10px; align-items: center;",
        actionButton(ns("add_query"), bsicons::bs_icon("plus-circle-fill"), class = "btn btn-success"),
        actionButton(ns("delete_query"), bsicons::bs_icon("trash-fill"), class = "btn btn-danger")
    ),
    actionButton(ns("run_query"), "Search"),
    tags$br(), 
    verbatimTextOutput(ns("query_result")),
    
    navset_card_tab(
      id = ns("qc_tabs"),
      nav_panel("Expression Patterns", uiOutput(ns("exp_patterns_pdf"))),
      nav_panel("Populations", uiOutput(ns("query_pops_pdf")))
      )

    )

}


##### SERVER #####

pop_search_server <- function(id, panel, all_markers, fsom_model, static_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cat("Population search server started for", id, "\n")
    
    # === Paths === #
    exp_patterns_path = file.path(static_dir, paste0("exp_patterns_", panel, ".pdf"))
    query_pops_path = file.path(static_dir, paste0("query_pops_", panel, ".pdf"))
    
    
    # === Warning if no model === #
    output$qc_warning <- renderUI({
      req(is.null(fsom_model()))
      tags$div(style = "color: red; font-weight: bold;", "No model found. Visit the 'Training' tab to create one first.")
    })
    
    # === Marker list === #
    markers <- all_markers[[panel]]
    
    
    # === Reactive IDs to keep track of search boxes === #
    query_ids <- reactiveVal(c(1))
    next_id <- reactiveVal(2)
    
    observeEvent(input$add_query, {
      current_ids <- query_ids()
      query_ids(c(current_ids, next_id()))
      next_id(next_id() + 1)
    })
    
    observeEvent(input$delete_query, {
      current_ids <- query_ids()
      if (length(current_ids) > 1) {
        query_ids(head(current_ids, -1))
      }
    })
    
    # === Reactive search boxes according to panel === #
    output$query_list_ui <- renderUI({
      lapply(query_ids(), function(i) {
        wellPanel(
          textInput(ns(paste0("pop_name_", i)), sprintf("Population %d Name", i), value = isolate(input[[paste0("pop_name_", i)]])),
          selectInput(ns(paste0("markers_", i)), "Select Markers", choices = markers, multiple = TRUE, selected = isolate(input[[paste0("markers_", i)]])),
          uiOutput(ns(paste0("marker_expr_", i)))
        )
      })
    })
    
    # === Dynamic marker expression levels === #
    observe({
      lapply(query_ids(), function(i) {
        local({
          ii <- i
          output[[paste0("marker_expr_", ii)]] <- renderUI({
            selected_markers <- input[[paste0("markers_", ii)]]
            if (is.null(selected_markers) || length(selected_markers) == 0) return(NULL)
            
            lapply(selected_markers, function(marker) {
              selectInput(ns(paste0("expr_", ii, "_", marker)),
                          label = paste("Expression of", marker),
                          choices = c("high", "low"), selected = isolate(input[[paste0("expr_", ii, "_", marker)]]))
            })
          })
        })
      })
    })
    
    
    # === Start search === #
    observeEvent(input$run_query, {
      shinyjs::disable("run_query")  # disable button
      
      tryCatch({
        # -- Formatting all inputs -- #
        ids <- query_ids()
        queries <- list()
        
        for (i in ids) {
          name <- input[[paste0("pop_name_", i)]]
          selected_markers <- input[[paste0("markers_", i)]]
          
          if (is.null(name) || name == "" || is.null(selected_markers)) next
          
          expression_values <- sapply(selected_markers, function(marker) {
            input[[paste0("expr_", i, "_", marker)]]
          }, USE.NAMES = TRUE)
          
          queries[[name]] <- expression_values
        }
        
        output$query_result <- renderPrint({
          if (length(queries) == 0) return("Please define at least one population to start search.")
        })
        
        # -- Run QueryMultiple and PlotVariable -- #
        model <- fsom_model()$model
        req(model)
        
        labels <- QueryMultiple(fsom = model,
                                cellTypes = queries,
                                plotFile = exp_patterns_path)
        
        PlotVariable(fsom = model, variable = labels) +
          theme(legend.text = element_text(size = 6),
                legend.key.size = unit(0.3, "cm"))
        
        ggsave(query_pops_path)
        cat("Finished plotting\n")
      },
      error = function(e) {
        showNotification(paste("Error during query:", conditionMessage(e)), type = "error")
      },
      finally = {
        shinyjs::enable("run_query")  # re-enable button
      })
    })
    
    
    
    # === Display results === #
    
    # -- Reactive file reader -- #
    exp_mod_time <- reactiveFileReader(
      intervalMillis = 2000,
      session = session,
      filePath = file.path("www", "flowsom", paste0("exp_patterns_", panel, ".pdf")),
      readFunc = function(path) file.info(path)$mtime)
    
    # -- Display expression patterns pdf -- #
    output$exp_patterns_pdf <- renderUI({
      time <- exp_mod_time()
      
      filePath = file.path("www", "flowsom", paste0("exp_patterns_", panel, ".pdf"))
      if (!file.exists(filePath)) {
        return(tags$div(style = "color: gray; font-style: italic;",
                        "Please define a population to search to see plots."))
      }
      
      rel_web_path <- file.path("flowsom", paste0("exp_patterns_", panel, ".pdf"))
      
      tags$iframe(
        src = sprintf("%s?time=%s", rel_web_path, as.numeric(time)),  # bust cache
        style = "width:100%; height:800px; border:none;"
      )
    })
    
    # -- Reactive file reader -- #
    pop_mod_time <- reactiveFileReader(
      intervalMillis = 2000,
      session = session,
      filePath = file.path("www", "flowsom", paste0("query_pops_", panel, ".pdf")),
      readFunc = function(path) file.info(path)$mtime)
    
    # -- Display populations pdf -- #
    output$query_pops_pdf <- renderUI({
      time <- pop_mod_time()  # reactive dependency
      
      filePath = file.path("www", "flowsom", paste0("query_pops_", panel, ".pdf"))
      if (!file.exists(filePath)) {
        return(tags$div(style = "color: gray; font-style: italic;",
                        "Please define a population to search to see plots."))
      }
      
      rel_web_path <- file.path("flowsom", paste0("query_pops_", panel, ".pdf"))
      
      tags$iframe(
        src = sprintf("%s?time=%s", rel_web_path, as.numeric(time)),  # bust cache
        style = "width:100%; height:800px; border:none;"
      )
    })
  })
}
