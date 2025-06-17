#######################################################
# Functions for viewing arcsinh transformation images #
#######################################################


##### IMPORTS #####

library(shiny)
library(bslib)
library(DT)



##### UI #####

transformation_image_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    h3("Transformation"),
    h4("FlowVS"),
    p("Fluorescent signals produced by flow cytometry are usually transformed by arcsinh function. 
      Transforming signal is necessary to compare values, for clustering and for visualization. 
      FlowVS predicts an optimal arcsinh cofactor for transformation that stabilizes variance per 
      fluorescent channel. However, based on biological knowledge of the marker's expression, 
      sometimes the best cofactor is not the one predicted by FlowVS and instead is chosen 
      to match the marker's expression."),
    
    selectInput(ns("condition"), "Select Condition:",
                choices = c("peanut", "unstim")),
    
    navset_card_tab(
      id = ns("transformation_tabs"),
      selected = "Before-After",
      
      nav_panel("Before-After",
                uiOutput(ns("before_after_ui"))
      ),
      
      nav_panel("Predictions",
                uiOutput(ns("flowvs_pdf_ui"))
      ),
      
      nav_panel("Cofactors",
                DTOutput(ns("cofactor_table"))
      )
    )
  )
}


##### SERVER #####

transformation_image_server <- function(id, panel_type, cofactor_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # === Before-After Marker Plots === #
    output$before_after_ui <- renderUI({
      req(input$condition)
      
      folder <- file.path("www", "transformation", panel_type, input$condition)
      validate(need(dir.exists(folder), paste("Folder not found:", folder)))
      image_files <- list.files(folder, pattern = "\\.png$", full.names = FALSE, ignore.case = TRUE)
      if (length(image_files) == 0) return(h5("No transformation images found."))
      
      # split into 2 columns
      rows <- split(image_files, ceiling(seq_along(image_files) / 2))
      tagList(
        lapply(rows, function(img_row) {
          fluidRow(
            lapply(img_row, function(img) {
              column(
                6,
                h5(tools::file_path_sans_ext(img)),
                tags$img(
                  src = file.path("transformation", panel_type, input$condition, img),
                  class = "img-fluid",
                  style = "width:100%; margin-bottom:20px;"
                )
              )
            })
          )
        })
      )
    })
    
    
    
    # === PDF Preview (FlowVS Cofactor Predictions) === #
    output$flowvs_pdf_ui <- renderUI({
      pdf_path <- normalizePath(file.path("www", "transformation", panel_type, input$condition, "cofactor_graphs.pdf"), mustWork = FALSE)
      if (!file.exists(pdf_path)) return(h5("PDF not found"))
      
      tags$iframe(
        src = file.path("transformation", panel_type, input$condition, "cofactor_graphs.pdf"),
        style = "width:100%; height:800px; border:none;"
      )
    })
    
    # === Cofactor Table === #
    output$cofactor_table <- renderDT({
      req(cofactor_data)
      df <- cofactor_data()
      validate(need(all(c("panel", "condition", "marker", "predicted", "used") %in% names(df)),
                    "Cofactor table missing required columns"))
      
      filtered <- dplyr::filter(df, panel == panel_type & condition == input$condition)
      datatable(filtered, options = list(pageLength = 10), rownames = FALSE)
    })
  })
}


