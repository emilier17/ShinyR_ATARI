###############################################
# Functions to check quality of FlowSOM model #
###############################################


##### IMPORTS #####

library(flowCore)
library(FlowSOM)
library(ggplot2)
library(ggpubr)
library(plotly)


##### UI #####

flowsom_qc_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Model Quality Control"),
    h4("FlowSOM"),
    p("Validate the trained model by viewing the different diagnostic plots below."),

    tags$div(style = "color: blue; font-weight: bold;", "Results may take a while to display!"),
    
    uiOutput(ns("qc_warning")),  # if no model exists yet
    
    # === Zoom controls === #
    tags$style(HTML("
  .zoom-wrapper {
    position: relative;
    overflow: auto;
    max-height: 700px;
    border: 1px solid #ccc;
  }

  .zoom-controls {
    position: absolute; 
    top: 40px;
    right: 40px;
    z-index: 1000;
    background-color: rgba(255, 255, 255, 0.8);
    padding: 8px;
    border-radius: 8px;
    box-shadow: 0 0 5px rgba(0,0,0,0.2);
  }

  .zoom-controls button {
    font-size: 1.1em;
    padding: 6px 12px;
    margin: 3px;
    cursor: pointer;
    background-color: #007BC2;
    color: white;
    border: none;
    border-radius: 4px;
  }

  .zoom-controls button:hover {
    background-color: #005c9a;
  }

  .zoom-image {
    transform-origin: top left;
    transition: transform 0.2s ease;
    cursor: default;
  }
")),
    
    # === Zoom mechanisms === #
    tags$script(HTML("
  let zoomScale = 1;

  Shiny.addCustomMessageHandler('zoom', function(data) {
  const img = document.getElementById(data.target);
  if (!img) return;

  let currentScale = parseFloat(img.getAttribute('data-zoom')) || 1;

  if (data.direction === 'in') {
    currentScale = Math.min(currentScale + 0.1, 5);
  } else if (data.direction === 'out') {
    currentScale = Math.max(currentScale - 0.1, 1);
  }

  img.setAttribute('data-zoom', currentScale);
  img.style.transform = `scale(${currentScale})`;
  });

")),
    
    # === Card tabs === #
    navset_card_tab(
      id = ns("qc_tabs"),
      
      nav_panel(tagList("Quick View", tooltip(bs_icon("question-circle-fill"), "Check the general aspect of the FlowSOM tree. The size of the clusters is relative to the amount of cells belonging to that clusters. Clusters that are physically close in the tree are more similar than distant clusters. The pie chart in the middle of the cluster shows the median expression of each marker in that cluster. Similar clusters are grouped together into different metaclusters (background halo color around each cluster).")),
                div(class = "zoom-wrapper",
                    div(class = "zoom-controls",
                        actionButton(ns("zoom_in_tree"), "+"),
                        actionButton(ns("zoom_out_tree"), "-")
                    ),
                    imageOutput(ns("tree_plot"), width = "100%", height = "auto")
                )
      ),
      
      
      nav_panel(tagList("Metacluster Coherence", tooltip(bs_icon("question-circle-fill"), "Check if clusters belonging to the same metacluster appear together in different marker plots.")),
                div(class = "zoom-wrapper",
                    div(class = "zoom-controls",
                        actionButton(ns("zoom_in_scatter"), "+"),
                        actionButton(ns("zoom_out_scatter"), "-")
                    ),
                    imageOutput(ns("scatter_plot"), width = "100%", height = "auto")
                )
      ),
      
      nav_panel(tagList("Batch Effect", tooltip(bs_icon("question-circle-fill"), "Assess if a batch effect contributed to cluster formation. The colored pies show the proportion of contributing batches to that cluster formation. If a large proportion of cells in a cluster come from a single batch, then a batch effect might be present.")),
                div(class = "zoom-wrapper",
                    div(class = "zoom-controls",
                        actionButton(ns("zoom_in_batch"), "+"),
                        actionButton(ns("zoom_out_batch"), "-")
                    ),
                    imageOutput(ns("batch_plot"), width = "100%", height = "auto")
                )
      ),
      nav_panel(tagList("Summary", tooltip(bs_icon("question-circle-fill"), "View the summary PDF produced by FlowSOM.")),
                uiOutput(ns("summary_pdf"))
      ),
    )
  )
}


##### SERVER #####

flowsom_qc_server <- function(id, results_dir, panel, static_dir, fsom_model) {
  moduleServer(id, function(input, output, session) {
    cat("Model QC server started for", id, "\n")
    ns <- session$ns
    
    # === Paths === #
    local_dir <- normalizePath(results_dir, winslash = "/")  # normalize for Windows
    shiny::addResourcePath("results", local_dir)
    metadata_path <- file.path(results_dir, "metadata.csv")
    summary_path = file.path(static_dir, paste0("summary_", panel, ".pdf"))
    
    
    # === Warning if no model === #
    output$qc_warning <- renderUI({
      req(is.null(fsom_model()))
      tags$div(style = "color: red; font-weight: bold;", "No model found. Visit the 'Training' tab to create one first.")
    })
    
    
    # === Zooms === #
    observeEvent(input$zoom_in_tree, {
      session$sendCustomMessage("zoom", list(target = ns("zoomable-tree"), direction = "in"))
    })
    observeEvent(input$zoom_out_tree, {
      session$sendCustomMessage("zoom", list(target = ns("zoomable-tree"), direction = "out"))
    })
    
    observeEvent(input$zoom_in_scatter, {
      session$sendCustomMessage("zoom", list(target = ns("zoomable-scatter"), direction = "in"))
    })
    observeEvent(input$zoom_out_scatter, {
      session$sendCustomMessage("zoom", list(target = ns("zoomable-scatter"), direction = "out"))
    })
    
    observeEvent(input$zoom_in_batch, {
      session$sendCustomMessage("zoom", list(target = ns("zoomable-batch"), direction = "in"))
    })
    observeEvent(input$zoom_out_batch, {
      session$sendCustomMessage("zoom", list(target = ns("zoomable-batch"), direction = "out"))
    })
    
    
    # === Quick view tab === #
    output$tree_plot <- renderImage({
      model <- fsom_model()$model
      req(model)
      
      stars <- FlowSOM::PlotStars(
        fsom = model,
        backgroundValues = model$metaclustering,)
      
      # temp png of the plot to be able to zoom 
      tmpfile <- tempfile(fileext = ".png")
      ggplot2::ggsave(tmpfile, stars, width = 10, height = 8, dpi = 300)
      
      list(
        src = tmpfile,
        contentType = "image/png",
        width = "100%",
        alt = "FlowSOM tree",
        class = "zoom-image",
        id = ns("zoomable-tree")
      )
    }, deleteFile = TRUE)
    
    
    # === Metacluser Coherence tab === #
    output$scatter_plot <- renderImage({
      model <- fsom_model()$model
      req(model)
      
      n_meta = length(levels(model$metaclustering))
      tmpfile <- tempfile(fileext = ".png")
      channel_pairs <- if (panel == "intracell") {
        list(
          c("CD4", "CD8"),         # T cells
          c("CD161", "CD45RA"),    # Memory
          c("CD137", "CD27"),      # Committed vs non-committed psT
          c("CD137", "CD154"),     # Activation
          c("IL4", "FSC-A"),       # IL-4
          c("IL5", "FSC-A"),       # IL-5
          c("IL9", "FSC-A"),       # IL-9
          c("IL13", "FSC-A"),      # IL-13
          c("IFNg", "FSC-A"),      # IFN-gamma
          c("CCR6", "CRTH2")       # Type 1 / Type 2
        )
      } else {
        list(
          c("CD4", "CD8"),         # T cells
          c("CD161", "CD45RA"),    # Memory
          c("CD137", "CD27"),      # Commited vs non-commited psT
          c("CD137", "CD154"),     # Activation
          c("CD25", "CD127")       # Tregs
        )
      }
      
      scatters = FlowSOM::Plot2DScatters(
        fsom = model,
        channelpairs = channel_pairs,
        metaclusters = seq_len(n_meta),
        clusters = NULL,
        plotFile = tmpfile)
      
      list(
        src = tmpfile,
        contentType = "image/png",
        width = "100%",
        alt = "Scatters plot",
        class = "zoom-image",
        id = ns("zoomable-scatter")
      )
    }, deleteFile = TRUE)
    
    
    # === Batch effect tab === #
    output$batch_plot <- renderImage({
      model <- fsom_model()$model
      req(model)
      req(file.exists(metadata_path))
      
      metadata <- read.csv(metadata_path, header=TRUE)
      
      fcs_files <- list.files(file.path(results_dir, "peanut"))
      cell_filenames = fcs_files[model$data[, "File"]]
      cleaned_filenames <- gsub("^Norm_", "", cell_filenames)
      cleaned_filenames <- gsub("_QC\\.fcs$", ".fcs", cleaned_filenames)
      filename_to_batch <- setNames(metadata$batch, metadata$filename)
      cell_batches = filename_to_batch[cleaned_filenames]
      cells_batches = factor(cell_batches)
      
      batch = FlowSOM::PlotPies(fsom = model, cellTypes = cell_batches)
      batch <- batch + ggplot2::labs(fill = "Batch number")
      
      tmpfile <- tempfile(fileext = ".png")
      ggplot2::ggsave(tmpfile, batch, width = 10, height = 8, dpi = 300)
      
      list(
        src = tmpfile,
        contentType = "image/png",
        width = "100%",
        alt = "Batch Effect",
        class = "zoom-image",
        id = ns("zoomable-batch")
      )
    }, deleteFile = TRUE)
    
    
    
    # === Summary FlowSOM tab === #
    summary_mod_time <- reactiveFileReader(
      intervalMillis = 2000,
      session = session,
      filePath = file.path("www", "flowsom", paste0("summary_", panel, ".pdf")),
      readFunc = function(path) file.info(path)$mtime)
    
    output$summary_pdf <- renderUI({
      time <- summary_mod_time()  # reactive dependency
      rel_web_path <- file.path("flowsom", paste0("summary_", panel, ".pdf"))
      
      tags$iframe(
        src = sprintf("%s?time=%s", rel_web_path, as.numeric(time)),  # bust cache
        style = "width:100%; height:800px; border:none;"
      )
    })
    
  })
}