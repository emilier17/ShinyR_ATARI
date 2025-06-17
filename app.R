#=============================================================================================#
# FlowSOM for analyzing high dimensional spectral flow cytometry (FC) data
# 
# Shiny R app for visualizing preprocessing and flowsom results
# 
# Author: Émilie Roy
# Date: June 2025
# Version: v.1.0
#
# Data:
# PBMC samples from ATARI trial (https://doi.org/10.3389/fmed.2023.1198173)
# Input: results from preprocessing and flowsom R scripts
# Output: Shiny R web app
#=============================================================================================#


##### IMPORTS #####

library(shiny)
library(bslib)
library(bsicons)
library(here)
library(shinycssloaders)
library(future)
library(furrr)

plan(multisession, workers = max(1, availableCores() - 3)) # parallelization setup 

source("global.R")
source("modules/gating_image_module.R")
source("modules/transformation_image_module.R")
source("modules/qc_image_module.R")
source("modules/normalization_image_module.R")
source("modules/flowsom_train_module.R")
source("modules/flowsom_qc_module.R")
source("modules/pop_search_module.R")
source("modules/marker_exp_module.R")
source("modules/diff_analysis_module.R")

setwd(here::here())
cat("App launched from:", getwd(), "\n")



##### UI ######

ui <- page_navbar(
  useShinyjs(), # for live message updates and disabling buttons
  
  # === Navbar === #
  title = "ATARI",
  id = "main_panel",
  navbar_options = navbar_options(bg = "#007BC2", underline = TRUE),
  header = tags$script(HTML("
  $(function() {
    $('.nav-link').on('shown.bs.tab', function (e) {
      Shiny.setInputValue('active_pill_tab', e.target.id, { priority: 'event' });
    });
  });
")),
  
  # === Sidebar for surface tab === #
  nav_panel(
    "Surface Panel",
    layout_sidebar(
      sidebar = sidebar(
        div(
          class = "nav flex-column nav-pills",
          id = "surface-tab",
          role = "tablist",
          `aria-orientation` = "vertical",
          
          tags$div("Preprocessing", style = "padding: 0.5rem 0.75rem; font-weight: bold; color: #666;"),
          tags$a("Gating", class = "nav-link active", id = "surface-Gating-tab", `data-bs-toggle` = "pill",
                 href = "#surface-Gating-tab-content", role = "tab", `aria-controls` = "surface-Gating-tab-content"),
          tags$a("Transformation", class = "nav-link", id = "surface-Transformation-tab", `data-bs-toggle` = "pill",
                 href = "#surface-Transformation-tab-content", role = "tab", `aria-controls` = "surface-Transformation-tab-content"),
          tags$a("QC", class = "nav-link", id = "surface-QC-tab", `data-bs-toggle` = "pill",
                 href = "#surface-QC-tab-content", role = "tab", `aria-controls` = "surface-QC-tab-content"),
          tags$a("Normalization", class = "nav-link", id = "surface-Normalization-tab", `data-bs-toggle` = "pill",
                 href = "#surface-Normalization-tab-content", role = "tab", `aria-controls` = "surface-Normalization-tab-content"),
          
          tags$div("FlowSOM", style = "padding: 0.5rem 0.75rem; font-weight: bold; color: #666;"),
          tags$a("Training", class = "nav-link", id = "surface_train-Training-tab", `data-bs-toggle` = "pill",
                 href = "#surface-Training-tab-content", role = "tab", `aria-controls` = "surface-Training-tab-content"),
          tags$a("Model QC", class = "nav-link", id = "surface-Model-QC-tab", `data-bs-toggle` = "pill",
                 href = "#surface-Model-QC-tab-content", role = "tab", `aria-controls` = "surface-Model-QC-tab-content"),
          tags$a("Population Search", class = "nav-link", id = "surface-pop-search-tab", `data-bs-toggle` = "pill",
                 href = "#surface-pop-search-tab-content", role = "tab", `aria-controls` = "surface-pop-search-tab-content"),
          tags$a("Marker Expression", class = "nav-link", id = "surface-marker-exp-tab", `data-bs-toggle` = "pill",
                 href = "#surface-marker-exp-tab-content", role = "tab", `aria-controls` = "surface-marker-exp-tab-content"),
          tags$a("Differential Analysis", class = "nav-link", id = "surface-diff-analysis-tab", `data-bs-toggle` = "pill",
                 href = "#surface-diff-analysis-tab-content", role = "tab", `aria-controls` = "surface-diff-analysis-tab-content")
        )
      ),
      div(
        class = "tab-content",
        id = "surface-tab-content",
        div(class = "tab-pane fade show active", id = "surface-Gating-tab-content", uiOutput("surface_gating")),
        div(class = "tab-pane fade", id = "surface-Transformation-tab-content", uiOutput("surface_transform")),
        div(class = "tab-pane fade", id = "surface-QC-tab-content", uiOutput("surface_qc")),
        div(class = "tab-pane fade", id = "surface-Normalization-tab-content", uiOutput("surface_norm")),
        div(class = "tab-pane fade", id = "surface-Training-tab-content", uiOutput("surface_train")),
        div(class = "tab-pane fade", id = "surface-Model-QC-tab-content", uiOutput("surface_model_qc")),
        div(class = "tab-pane fade", id = "surface-pop-search-tab-content", uiOutput("surface_pop_search")),
        div(class = "tab-pane fade", id = "surface-marker-exp-tab-content", uiOutput("surface_marker_exp")),
        div(class = "tab-pane fade", id = "surface-diff-analysis-tab-content", uiOutput("surface_diff_analysis"))
      )
    )
  ),
  
  # === Sidebar for intracell tab === #
  nav_panel(
    "Intracellular Panel",
    layout_sidebar(
      sidebar = sidebar(
        div(
          class = "nav flex-column nav-pills",
          id = "intracell-tab",
          role = "tablist",
          `aria-orientation` = "vertical",
          
          tags$div("Preprocessing", style = "padding: 0.5rem 0.75rem; font-weight: bold; color: #666;"),
          tags$a("Gating", class = "nav-link active", id = "intracell-Gating-tab", `data-bs-toggle` = "pill",
                 href = "#intracell-Gating-tab-content", role = "tab", `aria-controls` = "intracell-Gating-tab-content"),
          tags$a("Transformation", class = "nav-link", id = "intracell-Transformation-tab", `data-bs-toggle` = "pill",
                 href = "#intracell-Transformation-tab-content", role = "tab", `aria-controls` = "intracell-Transformation-tab-content"),
          tags$a("QC", class = "nav-link", id = "intracell-QC-tab", `data-bs-toggle` = "pill",
                 href = "#intracell-QC-tab-content", role = "tab", `aria-controls` = "intracell-QC-tab-content"),
          tags$a("Normalization", class = "nav-link", id = "intracell-Normalization-tab", `data-bs-toggle` = "pill",
                 href = "#intracell-Normalization-tab-content", role = "tab", `aria-controls` = "intracell-Normalization-tab-content"),
          
          tags$div("FlowSOM", style = "padding: 0.5rem 0.75rem; font-weight: bold; color: #666;"),
          tags$a("Training", class = "nav-link", id = "intracell_train-Training-tab", `data-bs-toggle` = "pill",
                 href = "#intracell-Training-tab-content", role = "tab", `aria-controls` = "intracell-Training-tab-content"),
          tags$a("Model QC", class = "nav-link", id = "intracell-Model-QC-tab", `data-bs-toggle` = "pill",
                 href = "#intracell-Model-QC-tab-content", role = "tab", `aria-controls` = "intracell-Model-QC-tab-content"),
          tags$a("Population Search", class = "nav-link", id = "intracell-pop-search-tab", `data-bs-toggle` = "pill",
                 href = "#intracell-pop-search-tab-content", role = "tab", `aria-controls` = "intracell-pop-search-tab-content"),
          tags$a("Marker Expression", class = "nav-link", id = "intracell-marker-exp-tab", `data-bs-toggle` = "pill",
                 href = "#intracell-marker-exp-tab-content", role = "tab", `aria-controls` = "intracell-marker-exp-tab-content"),
          tags$a("Differential Analysis", class = "nav-link", id = "intracell-diff-analysis-tab", `data-bs-toggle` = "pill",
                 href = "#intracell-diff-analysis-tab-content", role = "tab", `aria-controls` = "intracell-diff-analysis-tab-content")
        )
      ),
      div(
        class = "tab-content",
        id = "intracell-tab-content",
        div(class = "tab-pane fade show active", id = "intracell-Gating-tab-content", uiOutput("intracell_gating")),
        div(class = "tab-pane fade", id = "intracell-Transformation-tab-content", uiOutput("intracell_transform")),
        div(class = "tab-pane fade", id = "intracell-QC-tab-content", uiOutput("intracell_qc")),
        div(class = "tab-pane fade", id = "intracell-Normalization-tab-content", uiOutput("intracell_norm")),
        div(class = "tab-pane fade", id = "intracell-Training-tab-content", uiOutput("intracell_train")),
        div(class = "tab-pane fade", id = "intracell-Model-QC-tab-content", uiOutput("intracell_model_qc")),
        div(class = "tab-pane fade", id = "intracell-pop-search-tab-content", uiOutput("intracell_pop_search")),
        div(class = "tab-pane fade", id = "intracell-marker-exp-tab-content", uiOutput("intracell_marker_exp")),
        div(class = "tab-pane fade", id = "intracell-diff-analysis-tab-content", uiOutput("intracell_diff_analysis"))
      )
    )
  ),
  nav_spacer(),
  nav_menu(title = "Links",
           align = "right",
           nav_item(tags$a("GitHub", href = "https://github.com/emilier17/ShinyR_ATARI", target = "_blank")),
           nav_item(tags$a("Paper", href = "https://doi.org/10.3389/fmed.2023.1198173", target = "_blank"))),
)



###### SERVER #####

server <- function(input, output, session) {
  
  observe({
    cat(">> ACTIVE TAB:", input$active_pill_tab, "\n")
  })
  
  # === Marker lists === #
  marker_options <- list(
    surface = c("CD27", "CD45RA", "CD137", "CD161", "CD154", "CD25", "CD127", "ST2", "CRTH2", "CCR6", "PD1", "CTLA4"),
    intracell = c("CD27", "CD161", "CD45RA", "ST2", "CRTH2", "CCR6", "IL4", "IL5", "IL13", "IL9", "IFNg", "FoxP3")
  )
  
  marker_selected <- list(
    surface = c("CD25", "CD127", "CCR6", "CRTH2", "PD1", "CTLA4", "CD161", "CD27", "CD45RA", "CD154", "CD137"),
    intracell = c("CD137", "CD27", "CD161", "CD45RA", "CRTH2", "CCR6")
  )
  
  all_markers <- list(
    surface = c("CD154", "CD137", "CD25", "CD127", "ST2", "CCR6", "CRTH2", "PD1", "CTLA4", "CD161", "CD27", "CD45RA"),
    intracell = c("CD154", "CD137", "CD27", "CD161", "CD45RA", "ST2", "CRTH2", "CCR6", "IL4", "IL5", "IL9", "IL13", "IFNg", "FoxP3")
  )
  
  
  # === Reactive model loading and feature extraction === #
  
  # -- Surface model -- #
  fsom_model_surface <- reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = get_model_path("surface"),
    readFunc = function(path) {
      tryCatch({
        shinycssloaders::showPageSpinner(caption = "Loading surface FlowSOM model...")
        model <- readRDS(path)
        types <- c("counts", "percentages", "MFIs")
        markers <- all_markers$surface
        
        # PEANUT FILES #
        fcs_dir <- file.path("data", "surface", "peanut")
        filenames <- list.files(fcs_dir)
        files = file.path(fcs_dir, filenames)
        stopifnot(length(files) > 0, all(file.exists(files)))
        
        shinycssloaders::showPageSpinner(caption = "Extracting features from files... this can take a few minutes.")
        
        # parallel feature extraction
        peanut_list <- future_map2(
          .x = files,
          .y = filenames,
          .f = ~ tryCatch({
            out = FlowSOM::GetFeatures(fsom = model, files = .x, filenames = .y,
                                 type = types, MFI = markers)
            for (nm in names(out)) {
              rownames(out[[nm]]) <- .y
            }
            return(out)
          }, error = function(e) {
            cat("Skipped peanut file: ", .y)
            NULL
          }),
          .options = furrr_options(seed = TRUE)
        )
        peanut_list <- Filter(Negate(is.null), peanut_list)
        peanut_features <- Reduce(function(x, y) Map(rbind, x, y), peanut_list)
        lapply(peanut_features, rownames)
        
        # UNSTIM FILES #
        unstim_dir <- file.path("data", "surface", "unstim")
        unstim_files <- list.files(unstim_dir, full.names = TRUE)
        unstim_names <- basename(unstim_files)
        stopifnot(length(unstim_files) > 0, all(file.exists(unstim_files)))
        agg_dir <- file.path("data", "surface", "agg_unstim_surface.fcs")
        req(agg_dir)

        shinycssloaders::showPageSpinner(caption = "Loading and mapping unstimulated samples...")
        
        # load pre-aggregated unstim fcs files
        agg <- flowCore::read.FCS(
          filename = agg_dir,
          transformation = FALSE,
          truncate_max_range = FALSE
        )
        
        # mapping unstim files onto model
        fsom_unstim <- FlowSOM::NewData(fsom = model,
                                        input = agg,
                                        compensate = NULL,
                                        transform = NULL,
                                        toTransform = NULL,
                                        transformList = NULL,
                                        scale = NULL,
                                        silent = FALSE)

        shinycssloaders::showPageSpinner(caption = "Extracting unstimulated sample features...")
        
        # parallel feature extraction
        unstim_list <- future_map2(
          .x = unstim_files,
          .y = unstim_names,
          .f = ~ tryCatch({
            FlowSOM::GetFeatures(fsom = fsom_unstim, files = .x, filenames = .y,
                                 type = types, MFI = markers)
          }, error = function(e) {
            cat("Skipped unstim file: ", .y)
            NULL
          }),
          .options = furrr_options(seed = TRUE)
        )
        unstim_list <- Filter(Negate(is.null), unstim_list)
        unstim_features <- Reduce(function(x, y) Map(rbind, x, y), unstim_list)
        
        shinycssloaders::hidePageSpinner()
        
        list(
          model = model,
          features = peanut_features,
          unstim = list(
            fsom = fsom_unstim,
            features = unstim_features,
            filenames = unstim_names
          )
        )
      }, error = function(e) {
        shinycssloaders::hidePageSpinner()
        cat("Error loading FlowSOM surface model:", conditionMessage(e), "\n")
        NULL
      })
    }
  )
  
  # -- Intracell model -- #
  
  fsom_model_intracell <- reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = get_model_path("intracell"),
    readFunc = function(path) {
      tryCatch({
        shinycssloaders::showPageSpinner(caption = "Loading intracellular FlowSOM model...")
        model <- readRDS(path)
        types <- c("counts", "percentages", "MFIs")
        markers <- all_markers$intracell
        
        # PEANUT FILES #
        fcs_dir <- file.path("data", "intracell", "peanut")
        files <- list.files(fcs_dir, full.names = TRUE)
        filenames <- basename(files)
        stopifnot(length(files) > 0, all(file.exists(files)))
        
        shinycssloaders::showPageSpinner(caption = "Extracting features from files... this can take a few minutes.")
        
        # parallel feature extraction
        peanut_list <- future_map2(
          .x = files,
          .y = filenames,
          .f = ~ tryCatch({
            out = FlowSOM::GetFeatures(fsom = model, files = .x, filenames = .y,
                                 type = types, MFI = markers)
            for (nm in names(out)) {
              rownames(out[[nm]]) <- .y
            }
            out
          }, error = function(e) {
            cat("Skipped peanut file: ", .y)
            NULL
          }),
          .options = furrr_options(seed = TRUE)
        )
        peanut_list <- Filter(Negate(is.null), peanut_list)
        peanut_features <- Reduce(function(x, y) Map(rbind, x, y), peanut_list)
 
        
        # UNSTIM FILES #
        unstim_dir <- file.path("data", "intracell", "unstim")
        unstim_files <- list.files(unstim_dir, full.names = TRUE)
        unstim_names <- basename(unstim_files)
        stopifnot(length(unstim_files) > 0, all(file.exists(unstim_files)))
        agg_dir <- file.path("data", "intracell", "agg_unstim_intracell.fcs")
        req(agg_dir)

        shinycssloaders::showPageSpinner(caption = "Loading and mapping unstimulated samples...")
        
        # load pre-aggregated fcs files
        agg <- flowCore::read.FCS(
          filename = agg_dir,
          transformation = FALSE,
          truncate_max_range = FALSE
        )
        
        # map unstim data onto model
        fsom_unstim <- FlowSOM::NewData(fsom = model,
                                        input = agg,
                                        compensate = NULL,
                                        transform = NULL,
                                        toTransform = NULL,
                                        transformList = NULL,
                                        scale = NULL,
                                        silent = FALSE)

        shinycssloaders::showPageSpinner(caption = "Extracting unstimulated sample features...")
        
        # parallel feature extraction
        unstim_list <- future_map2(
          .x = unstim_files,
          .y = unstim_names,
          .f = ~ tryCatch({
            FlowSOM::GetFeatures(fsom = fsom_unstim, files = .x, filenames = .y,
                                 type = types, MFI = markers)
            for (nm in names(out)) {
              rownames(out[[nm]]) <- .y
            }
            out
          }, error = function(e) {
            cat("Skipped unstim file: ", .y)
            NULL
          }),
          .options = furrr_options(seed = TRUE)
        )
        unstim_list <- Filter(Negate(is.null), unstim_list)
        unstim_features <- Reduce(function(x, y) Map(rbind, x, y), unstim_list)

        
        shinycssloaders::hidePageSpinner()
        
        list(
          model = model,
          features = peanut_features,
          unstim = list(
            fsom = fsom_unstim,
            features = unstim_features,
            filenames = unstim_names
          )
        )
      }, error = function(e) {
        shinycssloaders::hidePageSpinner()
        cat("Error loading FlowSOM surface model:", conditionMessage(e), "\n")
        NULL
      })
    }
  )
  
  
  # === Surface UI outputs === #
  output$surface_gating        <- renderUI(gating_image_ui("surface_images"))
  output$surface_transform     <- renderUI(transformation_image_ui("surface_transform"))
  output$surface_qc            <- renderUI(qc_image_ui("surface_qc"))
  output$surface_norm          <- renderUI(normalization_image_ui("surface_normalization"))
  output$surface_train         <- renderUI(flowsom_train_ui("surface_train", "surface", marker_options, marker_selected))
  output$surface_model_qc      <- renderUI(flowsom_qc_ui("surface_model_qc"))
  output$surface_pop_search    <- renderUI(pop_search_ui("surface_pop_search"))
  output$surface_marker_exp    <- renderUI(marker_exp_ui("surface_marker_exp", all_markers, "surface"))
  output$surface_diff_analysis <- renderUI(diff_analysis_ui("surface_diff_analysis"))
  
  # === Intracell UI outputs === #
  output$intracell_gating        <- renderUI(gating_image_ui("intracell_images"))
  output$intracell_transform     <- renderUI(transformation_image_ui("intracell_transform"))
  output$intracell_qc            <- renderUI(qc_image_ui("intracell_qc"))
  output$intracell_norm          <- renderUI(normalization_image_ui("intracell_normalization"))
  output$intracell_train         <- renderUI(flowsom_train_ui("intracell_train", "intracell", marker_options, marker_selected))
  output$intracell_model_qc      <- renderUI(flowsom_qc_ui("intracell_model_qc"))
  output$intracell_pop_search    <- renderUI(pop_search_ui("intracell_pop_search"))
  output$intracell_marker_exp    <- renderUI(marker_exp_ui("intracell_marker_exp", all_markers, "intracell"))
  output$intracell_diff_analysis <- renderUI(diff_analysis_ui("intracell_diff_analysis"))
  
  # === Surface and Intracell server calls === #
  gating_image_server("surface_images", "surface", cell_count_data)
  gating_image_server("intracell_images", "intracell", cell_count_data)
  transformation_image_server("surface_transform", "surface", cofactor_data)
  transformation_image_server("intracell_transform", "intracell", cofactor_data)
  qc_image_server("surface_qc", "surface", peacoQC_data)
  qc_image_server("intracell_qc", "intracell", peacoQC_data)
  normalization_image_server("surface_normalization", "surface", marker_range_data)
  normalization_image_server("intracell_normalization", "intracell", marker_range_data)
  
  flowsom_train_server("surface_train",
                       fcs_path = file.path("data", "surface", "peanut"),
                       marker_options = marker_options,
                       marker_selected = marker_selected,
                       panel = "surface",
                       results_dir = file.path("data", "surface"),
                       static_dir = file.path("www", "flowsom"))
  
  flowsom_train_server("intracell_train",
                       fcs_path = file.path("data", "intracell", "peanut"),
                       marker_options = marker_options,
                       marker_selected = marker_selected,
                       panel = "intracell",
                       results_dir = file.path("data", "intracell"),
                       static_dir = file.path("www", "flowsom"))
  
  flowsom_qc_server("surface_model_qc",
                    results_dir = file.path("data", "surface"),
                    panel = "surface", 
                    static_dir = file.path("www", "flowsom"), 
                    fsom_model = fsom_model_surface)
  
  flowsom_qc_server("intracell_model_qc",
                    results_dir = file.path("data", "intracell"),
                    panel = "intracell", 
                    static_dir = file.path("www", "flowsom"), 
                    fsom_model = fsom_model_intracell)
  
  pop_search_server("surface_pop_search", "surface",
                    all_markers,
                    fsom_model = fsom_model_surface,
                    static_dir = file.path("www", "flowsom"))
  
  pop_search_server("intracell_pop_search", "intracell",
                    all_markers,
                    fsom_model = fsom_model_intracell,
                    static_dir = file.path("www", "flowsom"))
  
  marker_exp_server("surface_marker_exp", "surface",
                    all_markers,
                    fsom_model = fsom_model_surface,
                    static_dir = file.path("www", "flowsom"))
  
  marker_exp_server("intracell_marker_exp", "intracell",
                    all_markers,
                    fsom_model = fsom_model_intracell,
                    static_dir = file.path("www", "flowsom"))
  
  diff_analysis_server("surface_diff_analysis", "surface",
                       fsom_model_features = fsom_model_surface,
                       markers = all_markers,
                       static_dir = file.path("www", "flowsom"),
                       data_dir = file.path("data", "surface"))
  
  diff_analysis_server("intracell_diff_analysis", "intracell",
                       fsom_model_features = fsom_model_intracell,
                       markers = all_markers,
                       static_dir = file.path("www", "flowsom"),
                       data_dir = file.path("data", "intracell"))
}




###### LAUNCH APP #####

shinyApp(ui, server)

