##########################################
# Functions for viewing CytoNorm results #
##########################################


##### IMPORTS ######

library(shiny)
library(bslib)
library(DT)


##### UI #####

normalization_image_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    h3("Normalization"),
    h4("CytoNorm"),
    p("If the experiment was done in multiple batches, then a potential batch effect might arise.
    Instead of clustering based on biological differences, clustering might be performed based on batch
    differences. That's why its important to asses if a batch effect is present, and if so, correct it. 
    To detect and correct a batch effect, CytoNorm requires a technical replicate that was processed in 
    every batch. The technical replicate in this case is a PBMC sample taken from a single blood draw from 
    a single person. PBMCs aliquots were processed in every batch and treated the same as clinical samples."),
    
    selectInput(ns("condition"), "Select Condition:",
                choices = c("peanut", "unstim")),
    
    navset_card_tab(
      id = ns("normalization_tabs"),
      selected = "Before-After",
      
      nav_panel(tagList("Before-After",
                        tooltip(bs_icon("question-circle-fill"),
                                "See how normalization affected marker distribution. Each line represents all of a marker's values found in a single fcs file.")),
                uiOutput(ns("before_after_ui"))
      ),
      
      
      nav_panel(tagList("Splines",
                        tooltip(bs_icon("question-circle-fill"),
                                "See the splines used for normalization. One plot per batch and marker. Each row is an individual cluster used in the CytoNorm model. Black dots are quantiles in the model. Spline (red line) is the reference distribution of the marker learned from all batches.")),
                uiOutput(ns("splines_ui"))
      ),
      
      nav_panel(tagList("CV Test",
                        tooltip(bs_icon("question-circle-fill"),
                                "Check cluster and metacluser stability across control samples using CV (coefficient of variation). First page shows CVs for different numbers of metaclusters (rows). Second page shows percentages of cells assigned to each metacluster (columns) for every control samples (rows).")),
                uiOutput(ns("testCV_pdf_ui"))
      ),
      
      nav_panel(tagList("Marker Ranges",
                        tooltip(bs_icon("question-circle-fill"),
                                "See how normalization affected marker ranges. Only markers used for training the CytoNorm model will be affected by normalization.")),
                DTOutput(ns("marker_range_table"))
      )
    )
  )
}



##### SERVER #####

normalization_image_server <- function(id, panel_type, marker_range_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # === Before-After Marker Plots === #
    output$before_after_ui <- renderUI({
      req(input$condition)
      
      folder <- file.path("www", "normalization", panel_type, input$condition, "before_after")
      validate(need(dir.exists(folder), paste("Folder not found:", folder)))
      image_files <- list.files(folder, pattern = "\\.png$", full.names = FALSE, ignore.case = TRUE)
      if (length(image_files) == 0) return(h5("No normalized images found."))
      
      # split into 2 rows
      rows <- split(image_files, ceiling(seq_along(image_files) / 2))
      tagList(
        lapply(rows, function(img_row) {
          fluidRow(
            lapply(img_row, function(img) {
              column(
                6,
                h5(tools::file_path_sans_ext(img)),
                tags$img(
                  src = file.path("normalization", panel_type, input$condition, "before_after", img),
                  class = "img-fluid",
                  style = "width:100%; margin-bottom:20px;"
                )
              )
            })
          )
        })
      )
    })
    
    
    # === Marker Splines Plots === #
    output$splines_ui <- renderUI({
      req(input$condition)
      
      folder <- file.path("www", "normalization", panel_type, input$condition, "splines")
      validate(need(dir.exists(folder), paste("Folder not found:", folder)))
      image_files <- list.files(folder, pattern = "\\.png$", full.names = FALSE, ignore.case = TRUE)
      if (length(image_files) == 0) return(h5("No splines images found."))
      
      # split into 2 rows
      rows <- split(image_files, ceiling(seq_along(image_files) / 2))
      tagList(
        lapply(rows, function(img_row) {
          fluidRow(
            lapply(img_row, function(img) {
              column(
                6,
                h5(tools::file_path_sans_ext(img)),
                tags$img(
                  src = file.path("normalization", panel_type, input$condition, "splines", img),
                  class = "img-fluid",
                  style = "width:100%; margin-bottom:20px;"
                )
              )
            })
          )
        })
      )
    })
    
    
    # === PDF Preview (CytoNrom testCV results) === #
    output$testCV_pdf_ui <- renderUI({
      pdf_path <- normalizePath(file.path("www", "normalization", panel_type, input$condition, "testCV.pdf"), mustWork = FALSE)
      if (!file.exists(pdf_path)) return(h5("PDF not found"))
      
      tags$iframe(
        src = file.path("normalization", panel_type, input$condition, "testCV.pdf"),
        style = "width:100%; height:800px; border:none;"
      )
    })
    
    
    # === Marker Ranges Table === #
    output$marker_range_table <- renderDT({
      req(marker_range_data)
      df <- marker_range_data()
      validate(need(all(c("panel", "condition", "marker", "before_min", "before_max", "after_min", "after_max") %in% names(df)),
                    "Marker range table missing required columns"))
      
      filtered <- dplyr::filter(df, panel == panel_type & condition == input$condition)
      datatable(filtered, options = list(pageLength = 10), rownames = FALSE)
    })
  })
}
