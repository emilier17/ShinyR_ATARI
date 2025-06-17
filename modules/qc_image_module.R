#########################################
# Functions for viewing PeacoQC results #
#########################################


##### IMPORTS ######

library(shiny)
library(bslib)
library(DT)


##### UI ######

qc_image_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    h3("Quality Control"),
    h4("PeacoQC"),
    p("Remove low quality and abnormal events caused by flow cytometry acquisition issues 
      (clogs, flow rate changes...). PeacoQC detects poor quality events via two methods: 
      Isolation Tree (IT) and Mean Absolute Deviation (MAD)."),
    
    selectInput(ns("condition"), "Select Condition:",
                choices = c("peanut", "unstim", "control")),
    
    navset_card_tab(
      id = ns("qc_tabs"),
      selected = "Plots",
      
      nav_panel("Plots",
                textOutput(ns("sample_label")),
                tags$div(
                  style = "text-align: center;",
                  actionButton(ns("prev_btn"), "← Back"),
                  actionButton(ns("next_btn"), "Next →")
                ),
                tags$hr(),
                uiOutput(ns("plots_ui"))
      ),
      
      nav_panel("Summary",
                DTOutput(ns("peacoQC_table"))
      )
    )
  )
}


##### SERVER #####

qc_image_server <- function(id, panel_type, marker_range_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    
    # === Display PeacoQC png per file as carousel === #
    
    # -- Reactive file index -- #
    current_index <- reactiveVal(1)
    image_list <- reactive({
      folder <- file.path("www", "qc", panel_type, input$condition)
      imgs <- list.files(folder, pattern = "\\.png$", full.names = FALSE)
      current_index(1)  # reset index on condition change
      imgs
    })
    
    total_images <- reactive({ length(image_list()) })
    
    # -- Next image button -- #
    observeEvent(input$next_btn, {
      if (current_index() < total_images()) {
        current_index(current_index() + 1)
      }
    })
    
    # -- Previous image image -- #
    observeEvent(input$prev_btn, {
      if (current_index() > 1) {
        current_index(current_index() - 1)
      }
    })
    
    # -- Display file name above image -- #
    output$sample_label <- renderText({
      imgs <- image_list()
      if (length(imgs) == 0) return("No QC plots found.")
      paste0("Sample ", current_index(), " of ", length(imgs), ": ",
             tools::file_path_sans_ext(imgs[current_index()]))
    })
    
    # -- Display current image -- #
    output$plots_ui <- renderUI({
      imgs <- image_list()
      if (length(imgs) == 0) return(h5("No QC images available."))
      
      tags$img(
        src = paste("qc", panel_type, input$condition, imgs[current_index()], sep = "/"),
        style = "width: 100%; border: 1px solid #ccc; padding: 5px;"
      )
    })

    
    # === Display PeacoQC csv ===#
    output$peacoQC_table <- renderDT({
      req(peacoQC_data)
      df <- peacoQC_data()
      validate(need(all(c("panel", "condition", "sample", "cells_before", "cells_after", "perc_tot_removed", "perc_IT_removed", "perc_MAD_removed") %in% names(df)),
                    "Invalid marker summary table"))
      
      filtered <- dplyr::filter(df, panel == panel_type & condition == input$condition)
      datatable(filtered, options = list(pageLength = 10), rownames = FALSE)
    })
  })
}













