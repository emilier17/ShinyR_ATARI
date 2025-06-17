#######################################
# Functions for viewing gating images #
#######################################

##### IMPORTS #####

library(shiny)
library(bslib)
library(DT)


##### UI #####

gating_image_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    h3("Gating"),
    h4("FlowCore"),
    p("Gating is necessary to select only the cells of interest, removing debris, doublets 
      and dead cells. Here, the cells of interest are live single CD3+CD4+CD8- T cells."),
    
    selectInput(ns("condition"), "Select Condition:",
                choices = c("peanut", "unstim")),
    
    navset_card_tab(
      id = ns("gating_tabs"),
      
      nav_panel("Core Gating",
                fluidRow(
                  column(4, h5("Lymphocyte Gate"), imageOutput(ns("img_lymph"))),
                  column(4, h5("Singlet Gate"), imageOutput(ns("img_singlet"))),
                  column(4, h5("Live Cell Gate"), imageOutput(ns("img_live"))),
                  column(4, h5("CD3+ Gate"), imageOutput(ns("img_CD3"))),
                  column(4, h5("CD4+CD8- Gate"), imageOutput(ns("img_CD4")))
                )
      ),
      
      nav_panel("Panel-Specific",
               uiOutput(ns("extra_images_ui"))
      ),
      
      nav_panel("Cell Count",
                DTOutput(ns("cell_count_table"))
      )
    )
  )
}

##### SERVER #####

gating_image_server <- function(id, panel_type, cell_count_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # === Core gating images === #
    get_image_path <- function(gate_name) {
      normalizePath(file.path("www", "gating", panel_type, input$condition, paste0(gate_name, ".png")))
    }
    
    render_gate_image <- function(gate_name) {
      renderImage({
        path <- get_image_path(gate_name)
        validate(need(file.exists(path), paste("Missing image:", path)))
        list(src = path, contentType = "image/png", width = "100%")
      }, deleteFile = FALSE)
    }
    
    output$img_lymph   <- render_gate_image("lymphocyte")
    output$img_singlet <- render_gate_image("singlets")
    output$img_live    <- render_gate_image("live")
    output$img_CD3    <- render_gate_image("CD3")
    output$img_CD4    <- render_gate_image("CD4")
    
    
    # === Panel-specific images === #
    output$extra_images_ui <- renderUI({
      folder <- file.path("www", "gating", panel_type, input$condition)
      validate(need(dir.exists(folder), "Image folder missing"))
      
      all_imgs <- list.files(folder, pattern = "\\.png$", full.names = TRUE)
      core_imgs <- c("lymphocyte.png", "singlets.png", "live.png", "CD3.png", "CD4.png")
      extra_imgs <- all_imgs[!basename(all_imgs) %in% core_imgs]
      
      if (length(extra_imgs) == 0) {
        return(h5("No additional images found."))
      }
      
      # Split into groups of 3
      rows <- split(extra_imgs, ceiling(seq_along(extra_imgs) / 3))
      
      tagList(
        lapply(rows, function(img_row) {
          fluidRow(
            lapply(img_row, function(img) {
              column(
                4,
                h5(tools::file_path_sans_ext(basename(img))),
                tags$img(
                  src = file.path("gating", panel_type, input$condition, basename(img)),
                  style = "width:100%; margin-bottom:20px;"
                )
              )
            })
          )
        })
      )
    })
    
    
    # === Cell Count Table === #
    output$cell_count_table <- renderDT({
      req(cell_count_data)
      df <- cell_count_data()  # should return a data.frame
      validate(need(ncol(df) >= 2, "Invalid cell count data"))
      
      filtered <- dplyr::filter(df, panel == panel_type & condition == input$condition)
      datatable(filtered, options = list(pageLength = 10), rownames = FALSE)
    })
    
  })
}

