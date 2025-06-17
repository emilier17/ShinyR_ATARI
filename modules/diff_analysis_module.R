###################################################################################################################
# Functions to perform differential analysis between experimental conditions (timepoints, treatment, stimulation) #
###################################################################################################################


##### IMPORTS #####

library(FlowSOM)
library(ggplot2)
library(ggpubr)
library(shinyjs)
library(stringr)
library(shinycssloaders)


##### UI #####

diff_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Differential Analysis"),
    h4("FlowSOM"),
    p("Perform differential analysis based on different FlowSOM tree features between experimental conditions.
      Wilcoxon test is performed. Clusters or metaclusters that are signficantly different between groups and
      that have a big enough fold change will be highlighted in the FlowSOM tree."),
    
    uiOutput(ns("qc_warning")),  # if no model exists yet
    
    
    selectInput(ns("feature_type"),
                label = tagList("Select Feature to Compare",
                                tooltip(bs_icon("question-circle-fill"),"The feature to compare between groups.
                                        Counts: the number of cells assigned to a cluster or metacluster.
                                        Percentages: the percentage of total cells assigned to a cluster or metacluster.
                                        MFIs: the fluorescence value for a marker in a cluster or metacluster (mean fluorescence intensity).")),
                choices = c("counts", "percentages", "MFIs"), selected = "percentages"),

    
    selectInput(ns("granularity"),
                label = tagList("Select Granularity",
                                tooltip(bs_icon("question-circle-fill"), "Should the feature be measured in clusters or metaclusters.")),
                choices = c("cluster", "metacluster"), selected = "cluster"),
    
    
    selectInput(ns("comparison_groups"),
                label = tagList("Select Groups to Compare",
                                tooltip(bs_icon("question-circle-fill"), "Which conditions to compare.
                                        Timepoints: PBMC samples were taken at seven different timepoints
                                        (baseline, weeks 2, 6, 12, 24, 36, 48). Treatments: control (placebo+immunotherapy)
                                        or active (abatacept+immunotherapy) arm. Timepoints+Treatments: compare the same
                                        timepoints between treatment groups. Stimulation: PBMC samples were either
                                        stimulated with peanut extract or left unstimulated.")),
                choices = c("timepoints", "treatments", "timepoints+treatments", "stimulation"), selected="Timepoints"),
    
    
    uiOutput(ns("feature_specific_inputs")),
    
    
    selectInput(ns("fold_change"),
                label = tagList("Select Fold Change Measure",
                                tooltip(bs_icon("question-circle-fill"), "Which measure to quantify the difference between group's feature.")),
                choices = c("fold changes", "log10 fold changes"), selected="fold changes"),
    
    
    numericInput(ns("fold_change_thresh"),
                 label = tagList("Difference Threshold",
                                 tooltip(bs_icon("question-circle-fill"), "Value of fold change that is considered
                                         'big enough' for the difference between the feature's values of the groups
                                         to be considered interesting.")),
                 value = 2.0, min = 0.1, step = 0.1) %>% shiny::tagAppendAttributes(required = "true"),
    
    
    selectInput(ns("p_value_type"),
                label = tagList("P-value Type",
                                tooltip(bs_icon("question-circle-fill"), "Format of the p-value produced by the Wilcoxon test.")),
                choices = c("p values", "adjusted p values", "-log10 p values"), selected = "adjusted p values"),
    
    
    numericInput(ns("p_value_threshold"),
                 label = tagList("Significance Threshold",
                                 tooltip(bs_icon("question-circle-fill"), "Threshold p-value to consider the Wilcoxon test significant")),
                 value = 0.05, min = 0, step = 0.01) %>% shiny::tagAppendAttributes(required = "true"),
    
    
    actionButton(ns("run_diff"), "Run Analysis"),
    
    tags$hr(),
    
    tags$h5("Analysis Log"),
    verbatimTextOutput(ns("analysis_log")),
    
    navset_card_tab(
      id = ns("qc_tabs"),
      nav_panel("Tree Plot", uiOutput(ns("diff_analysis_pdf"))),
      nav_panel("All Results", uiOutput(ns("csv_warning")), DTOutput(ns("diff_analysis_csv")))
    )
  )
}


##### SERVER #####

diff_analysis_server <- function(id, panel, fsom_model_features, markers, static_dir, data_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cat("Differential analysis server started for", id, "\n")
    
    # === Adaptive UI according to feature_type or comparison_groups selection === #
    output$feature_specific_inputs <- renderUI({
      ft <- input$feature_type
      cg <- input$comparison_groups
      
      input_list <- list()
      
      # -- Show MFI marker selectInput when feature_type is MFIs -- #
      if (ft == "MFIs") {
        marker_choices <- switch(panel,
                                 "surface" = markers$surface,
                                 "intracell" = markers$intracell,
                                 character(0))
        input_list <- append(input_list, list(
          selectInput(ns("mfi_marker"),
                      label = "Select Marker for MFI",
                      choices = marker_choices)
        ))
      }
      
      # -- Timepoint options for comparison -- #
      tp_levels <- c("BAS", "W2", "W6", "W12", "W24", "W36", "W48")
      
      if (cg =="timepoints") {
        input_list <- append(input_list, list(
          selectInput(ns("ref_tp"),
                      label = "Reference Timepoint",
                      choices = tp_levels, selected = "BAS"),
          selectInput(ns("comp_tp"),
                      label = "Comparison Timepoint",
                      choices = setdiff(tp_levels, "BAS"))
        ))
      }
      
      if (cg =="timepoints+treatments") {
        treat_levels <- c("placebo", "abatacept")
        combo_levels <- as.vector(outer(tp_levels, treat_levels, paste, sep = "_"))
        
        input_list <- append(input_list, list(
          selectInput(ns("ref_tp_treat"),
                      label = "Reference Timepoint + Treatment",
                      choices = combo_levels, selected = "BAS_placebo"),
          selectInput(ns("comp_tp_treat"),
                      label = "Comparison Timepoint + Treatment",
                      choices = setdiff(combo_levels, "BAS_placebo"))
        ))
      }
      
      if (length(input_list) > 0) tagList(input_list)
    })
    
    
    
    # === Warning if no model === #
    output$qc_warning <- renderUI({
      req(is.null(fsom_model_features()))
      tags$div(style = "color: red; font-weight: bold;",
               "No model found. Visit the 'Training' tab to create one first.")
    })
    
    
    # === Live updates messages with shinyjs setup === #
    log_text <- reactiveVal("")
    

    # === Run differential analysis === #
    observeEvent(input$run_diff, {
      shinyjs::disable("run_diff") # disable button
      shinyjs::html(id = "analysis_log", "")  # clear log
      
      tryCatch ({
        withCallingHandlers({
          message("Started differential analysis, this might take a while.")
        
          # -- User choices -- #
          feature_type = input$feature_type
          granularity = input$granularity
          feature = paste0(granularity, "_", feature_type)
          stat = input$fold_change
          groups = input$comparison_groups
          stat_threshold = as.numeric(input$fold_change_thresh)
          significance = input$p_value_type
          sign_threshold = as.numeric(input$p_value_threshold)
          req(c(feature_type, granularity, feature, stat, groups, stat_threshold, significance, sign_threshold))
          
          cat("Stat_threshold:", stat_threshold, "Class:", class(stat_threshold), "\n")
          cat("Sign_threshold:", sign_threshold, "Class:", class(sign_threshold), "\n")
          
          # -- Necessary parameters -- #
          message(">> Loading input parameters...")
          req(fsom_model_features())
          model <- fsom_model_features()$model
          features = fsom_model_features()$features
          markers_MFIs = markers$panel
          filenames = list.files(file.path(data_dir, "peanut"))
  
          
          # -- Defining comparison groups -- #
          timepoints = c("BAS", "W2", "W6", "W12", "W24", "W36", "W48")
          timepoints_dec <- timepoints[order(nchar(timepoints), decreasing = TRUE)] # to avoid W24 being associated to W2
          participant_ids = str_extract(filenames, "AA[0-9]{5}")
          abatacept = c("AA22011", "AA22019", "AA23001", "AA23003", "AA23005", "AA23008", "AA23013")
          placebo = c("AA22008", "AA22020", "AA23002", "AA23004", "AA23009", "AA23010", "AA23015")
          
          extracted_timepoints = str_extract(filenames, str_c(timepoints_dec, collapse = "|"))
          map_id_treatment = ifelse(participant_ids %in% placebo, "placebo",
                                    ifelse(participant_ids %in% abatacept, "abatacept", NA))
          time_treat_labels = paste(extracted_timepoints, map_id_treatment, sep = "_")
          
          timepoint_groups = split(filenames, extracted_timepoints)
          treatment_groups = split(filenames, map_id_treatment)
          time_treat_groups = split(filenames, time_treat_labels)
          
          
          #-- Differential analysis function -- #
          message(">> Defining differential analysis function...")
          
          diff_analysis_plot <- function(fsom, features, feature, stat, groups, ref, comp,
                                         colors = c("blue", "red", "gray"),
                                         stat_threshold = 2.5,
                                         significance = "adjusted p values",
                                         sign_threshold = 0.05,
                                         custom_CVS_name = NULL){
            
            # Creating legend and colors setup
            if (ref == comp) stop("ref and comp groups must be different")
            
            stat_levels = c(paste0(comp, " underrepresented compared to ", ref),
                            paste0(ref, " underrepresented compared to ", comp),
                            "No significant difference")
            names(colors) = stat_levels
            
            
            # Calculate statistics (Wilcox test with Benjamini Hochberg corrected p-value)
            if (!(ref %in% names(groups)) || !(comp %in% names(groups))) {
              stop("ref and/or comp was not found in the given groups")}
            
            cat("Checking group match:\n")
            cat("ref =", ref, "comp =", comp, "\n")
            cat("Group names =", names(groups), "\n")
            cat("Feature rownames (first 5):", head(rownames(features[[feature]])), "\n")
            cat("Group ref files (first 5):", head(groups[[ref]]), "\n")
            
            stats <- FlowSOM::GroupStats(features = features[[feature]], groups = groups[c(ref, comp)])
            cluster_stat_raw = stats[stat, ]
            p = stats[significance, ]
            cat("Length of p:", length(p), "\n")
            cat("Length of cluster_stat_raw:", length(cluster_stat_raw), "\n")
            
            
            # Convert to categorical stat map
            cluster_stat = factor(ifelse(p < sign_threshold & cluster_stat_raw < -stat_threshold, stat_levels[1],
                                         ifelse(p < sign_threshold & cluster_stat_raw > stat_threshold, stat_levels[2], stat_levels[3])),
                                  levels = stat_levels)
            cluster_stat[is.na(cluster_stat)] = stat_levels[3]
            cluster_stat = factor(cluster_stat, levels = stat_levels)
            cat("Length of cluser_stat:", length(cluster_stat), "\n")
            colors_named = setNames(colors, stat_levels)
            
            
            # Saving stats to cvs
            transposed_stats = t(stats)
            write.csv(transposed_stats, file.path(static_dir, paste0("diff_analysis_", panel, ".csv")), row.names = TRUE)
    
            
            # Dummy legend in case of any of the cluster_stat = 0
            cluster_stat_for_legend = cluster_stat
            if (sum(cluster_stat == stat_levels[1]) == 0) {
              cluster_stat_for_legend[which(cluster_stat == stat_levels[3])[1]] <- stat_levels[1]
            }
            if (sum(cluster_stat == stat_levels[2]) == 0) {
              cluster_stat_for_legend[which(cluster_stat == stat_levels[3])[2]] <- stat_levels[2]
            }
            cluster_stat_for_legend = factor(cluster_stat_for_legend, levels = stat_levels)
            
            
            # If granularity is metaclusters, apply metacluster values to clusters for plotting
            expand_to_clusters <- function(vals, fsom) {
              n_clusters = length(fsom$metaclustering)
              n_metaclusters = length(unique(fsom$metaclustering))
              
              cat("expand_to_clusters(): length(vals) =", length(vals), "\n")
              cat("Expected: clusters =", n_clusters, ", metaclusters =", n_metaclusters, "\n")
                
              if (length(vals) == n_clusters) {
                return(vals)
              } 
              else if (length(vals) == n_metaclusters) {
                metacluster_assignments <- as.integer(fsom$metaclustering)
                return(vals[metacluster_assignments])
              }
              else {
                stop(sprintf("Unexpected vector length in expand_to_clusters: got %d, expected %d (clusters) or %d (metaclusters)",
                             length(vals), n_clusters, n_metaclusters))
              }
            }
            
            ref_vals <- expand_to_clusters(stats[paste0("medians ", ref), ], fsom)
            comp_vals <- expand_to_clusters(stats[paste0("medians ", comp), ], fsom)
            bg_vals   <- expand_to_clusters(cluster_stat, fsom)
            bg_legend <- expand_to_clusters(cluster_stat_for_legend, fsom)
            
            
            # Plotting
            gr_1 <- PlotStars(
              fsom = fsom,
              title = ref,
              nodeSizes = ref_vals,
              backgroundValues = bg_vals,
              backgroundColors = colors_named,
              list_insteadof_ggarrange = TRUE)
            
            gr_2 <- PlotStars(
              fsom = fsom,
              title = comp,
              nodeSizes = comp_vals,
              backgroundValues = bg_vals,
              backgroundColors = colors_named,
              list_insteadof_ggarrange = TRUE)
            
            # Dummy legend with forced full colorlevels
            gr_legend <- PlotStars(
              fsom = fsom,
              title = "Legend",
              backgroundValues = bg_legend,
              backgroundColors = colors_named,
              list_insteadof_ggarrange = TRUE)
            
            dev.off()
            
            return(list(gr_1$tree, gr_2$tree, gr_legend$starLegend, gr_legend$backgroundLegend))
          }
         
          
          #-- MFI chosen-- #
          if (feature_type == "MFIs") {
            message(">> Running differential analysis...")
            
            req(input$mfi_marker)
            mk = input$mfi_marker
            prefix = if (granularity == "cluster") {
              "C[0-9]+"
            } else if (granularity == "metacluster") {
              "MC[0-9]+"
            } else {
              stop("unknown granularity")
            }
            pattern <- paste0("^", prefix, " ", mk, " <")
            selected_cols <- grep(pattern, colnames(features[[feature]]), value = TRUE)
            subset_feature <- features[[feature]][, selected_cols, drop=FALSE]
            mk_features = list()
            mk_features[[feature]] = subset_feature
  
            
            # TIMEPOINTS COMPARISON #
            if (groups == "timepoints") {
              ref <- input$ref_tp
              comp <- input$comp_tp
              
              plot <- diff_analysis_plot(fsom = model,
                                         features = mk_features,
                                         feature = feature,
                                         stat = stat,
                                         stat_threshold = stat_threshold,
                                         significance = significance,
                                         sign_threshold = sign_threshold,
                                         groups = timepoint_groups,
                                         ref = ref,
                                         comp = comp)
              
              message(">> Saving plot...")
              pdf(file.path(static_dir, paste0("diff_analysis_", panel, ".pdf")))
              print(plot[[3]])
              page_plot <- ggpubr::ggarrange(plotlist = list(plot[[1]],
                                                             plot[[2]],
                                                             plot[[4]]),
                                             heights = c(3, 1))
              print(page_plot)
              dev.off()
              message(">> Differential analysis complete. Plot and CSV now ready.")
              return()
            }
            
            # TREATMENTS COMPARISON #
            else if (groups == "treatments") {
              plot <- diff_analysis_plot(fsom = model,
                                         features = mk_features,
                                         feature = feature,
                                         stat = stat,
                                         stat_threshold = stat_threshold,
                                         significance = significance,
                                         sign_threshold = sign_threshold,
                                         groups = treatment_groups,
                                         ref = "placebo",
                                         comp = "abatacept")
              
              message(">> Saving plot...")
              pdf(file.path(static_dir, paste0("diff_analysis_", panel, ".pdf")))
              print(plot[[3]])
              page_plot <- ggpubr::ggarrange(plotlist = list(plot[[1]],
                                                             plot[[2]],
                                                             plot[[4]]),
                                             heights = c(3, 1))
              print(page_plot)
              dev.off()
              message(">> Differential analysis complete. Plot and CSV now ready.")
              return()
            }
            
            # TIMEPOINTS + TREATMENTS COMPARISON #
            else if (groups == "timepoints+treatments") {
              ref <- input$ref_tp_treat
              comp <- input$comp_tp_treat
              
              plot <- diff_analysis_plot(fsom = model,
                                         features = mk_features,
                                         feature = feature,
                                         stat = stat,
                                         stat_threshold = stat_threshold,
                                         significance = significance,
                                         sign_threshold = sign_threshold,
                                         groups = time_treat_groups,
                                         ref = ref,
                                         comp = comp)
              
              message(">> Saving plot...")
              pdf(file.path(static_dir, paste0("diff_analysis_", panel, ".pdf")))
              print(plot[[3]])
              page_plot <- ggpubr::ggarrange(plotlist = list(plot[[1]],
                                                             plot[[2]],
                                                             plot[[4]]),
                                             heights = c(3, 1))
              print(page_plot)
              dev.off()
              message(">> Differential analysis complete. Plot and CSV now ready.")
              return()
            }
            
            # STIMULATION COMPARISON #
            else if (groups == "stimulation") {
              # importing unstim features
              req(fsom_model_features()$unstim)
              unstim <- fsom_model_features()$unstim
              
              # combining peanut features + unstim features
              complete_features <- list(
                cluster_percentages = rbind(features[["cluster_percentages"]], unstim$features[["cluster_percentages"]]),
                cluster_counts = rbind(features[["cluster_counts"]], unstim$features[["cluster_counts"]]),
                cluster_MFIs = rbind(features[["cluster_MFIs"]], unstim$features[["cluster_MFIs"]]),
                metacluster_percentages = rbind(features[["metacluster_percentages"]], unstim$features[["metacluster_percentages"]]),
                metacluster_counts = rbind(features[["metacluster_counts"]], unstim$features[["metacluster_counts"]]),
                metacluster_MFIs = rbind(features[["metacluster_MFIs"]], unstim$features[["metacluster_MFIs"]])
              )
              
              # selecting only the data for the chosen marker (mk)
              prefix = if (granularity == "cluster") {
                "C[0-9]+"
              } else if (granularity == "metacluster") {
                "MC[0-9]+"
              } else {
                stop("unknown granularity")
              }
              pattern <- paste0("^", prefix, " ", mk, " <")
              selected_cols <- grep(pattern, colnames(complete_features[[feature]]), value = TRUE)
              subset_feature <- complete_features[[feature]][, selected_cols, drop=FALSE]
              mk_features = list()
              mk_features[[feature]] = subset_feature
              
              #creating group list
              control_groups <- list("peanut" = filenames, "unstim" = unstim$filenames)
              control_groups
              
              #diff analysis plots
              plot <- diff_analysis_plot(fsom = model,
                                         features = mk_features,
                                         feature = feature,
                                         stat = stat,
                                         stat_threshold = stat_threshold,
                                         significance = significance,
                                         sign_threshold = sign_threshold,
                                         groups = control_groups,
                                         ref = "peanut",
                                         comp = "unstim")
              
              message(">> Saving plot...")
              pdf(file.path(static_dir, paste0("diff_analysis_", panel, ".pdf")))
              print(plot[[3]])
              page_plot = ggpubr::ggarrange(plotlist = list(plot[[1]],
                                                            plot[[2]],
                                                            plot[[4]]),
                                            heights=c(3,1))
              print(page_plot)
              message(">> Differential analysis complete. Plot and csv now ready.")
              dev.off()
              return()
            }
            
            else {
              stop("Unknown comparison group in MFIs block.")
            }
            return()
          }
  
          
          #-- Timepoints chosen-- #
          if (groups == "timepoints") {
            message(">> Running differential analysis...")
            ref = input$ref_tp
            comp = input$comp_tp
            
            plot <- diff_analysis_plot(fsom = model,
                                       features = features,
                                       feature = feature,
                                       stat = stat,
                                       stat_threshold = stat_threshold,
                                       significance = significance,
                                       sign_threshold = sign_threshold,
                                       groups = timepoint_groups,
                                       ref = ref,
                                       comp = comp)
            
            message(">> Saving plot...")
            pdf(file.path(static_dir, paste0("diff_analysis_", panel, ".pdf")))
            print(plot[[3]])
            page_plot = ggpubr::ggarrange(plotlist = list(plot[[1]],
                                                          plot[[2]],
                                                          plot[[4]]),
                                          heights=c(3,1))
            print(page_plot)
            message(">> Differential analysis complete. Plot and csv now ready.")
            dev.off()
            return()
          }
          
          
          #-- Treatments chosen-- #
          if (groups == "treatments") {
            message(">> Running differential analysis...")
            
            plot <- diff_analysis_plot(fsom = model,
                                       features = features,
                                       feature = feature,
                                       stat = stat,
                                       stat_threshold = stat_threshold,
                                       significance = significance,
                                       sign_threshold = sign_threshold,
                                       groups = treatment_groups,
                                       ref = "placebo",
                                       comp = "abatacept")
            
            message(">> Saving plot...")
            pdf(file.path(static_dir, paste0("diff_analysis_", panel, ".pdf")))
            print(plot[[3]])
            page_plot = ggpubr::ggarrange(plotlist = list(plot[[1]],
                                                          plot[[2]],
                                                          plot[[4]]),
                                          heights=c(3,1))
            print(page_plot)
            message(">> Differential analysis complete. Plot and csv now ready.")
            dev.off()
            return()
          }
          
          
          #-- Timepoints + Treatments chosen-- #
          if (groups == "timepoints+treatments") {
            message(">> Running differential analysis...")
            ref <- input$ref_tp_treat
            comp <- input$comp_tp_treat
            
            plot <- diff_analysis_plot(fsom = model,
                                       features = features,
                                       feature = feature,
                                       stat = stat,
                                       stat_threshold = stat_threshold,
                                       significance = significance,
                                       sign_threshold = sign_threshold,
                                       groups = time_treat_groups,
                                       ref = ref,
                                       comp = comp)
      
            message(">> Saving plot...")
            pdf(file.path(static_dir, paste0("diff_analysis_", panel, ".pdf")))
            print(plot[[3]])
            page_plot = ggpubr::ggarrange(plotlist = list(plot[[1]],
                                                          plot[[2]],
                                                          plot[[4]]),
                                          heights=c(3,1))
            print(page_plot)
            message(">> Differential analysis complete. Plot and csv now ready.")
            dev.off()
            return()
          }
          
  
          # STIMULATION
          if (groups == "stimulation") {
            # loading unstim features
            req(fsom_model_features()$unstim)
            unstim <- fsom_model_features()$unstim
            
            # combining peanut features and unstim features
            complete_features <- list(
              cluster_percentages = rbind(features[["cluster_percentages"]], unstim$features[["cluster_percentages"]]),
              cluster_counts = rbind(features[["cluster_counts"]], unstim$features[["cluster_counts"]]),
              cluster_MFIs = rbind(features[["cluster_MFIs"]], unstim$features[["cluster_MFIs"]]),
              metacluster_percentages = rbind(features[["metacluster_percentages"]], unstim$features[["metacluster_percentages"]]),
              metacluster_counts = rbind(features[["metacluster_counts"]], unstim$features[["metacluster_counts"]]),
              metacluster_MFIs = rbind(features[["metacluster_MFIs"]], unstim$features[["metacluster_MFIs"]])
            )
  
            #creating group list
            control_groups <- list("peanut" = filenames, "unstim" = unstim$filenames)
            control_groups
            
            #diff analysis plots
            message(">> Running differential analysis...")
            
            plot <- diff_analysis_plot(fsom = model,
                                       features = complete_features,
                                       feature = feature,
                                       stat = stat,
                                       stat_threshold = stat_threshold,
                                       significance = significance,
                                       sign_threshold = sign_threshold,
                                       groups = control_groups,
                                       ref = "peanut",
                                       comp = "unstim")
            
            message(">> Saving plot...")
            pdf(file.path(static_dir, paste0("diff_analysis_", panel, ".pdf")))
            print(plot[[3]])
            page_plot = ggpubr::ggarrange(plotlist = list(plot[[1]],
                                                          plot[[2]],
                                                          plot[[4]]),
                                          heights=c(3,1))
            print(page_plot)
            message(">> Differential analysis complete. Plot and csv now ready.")
            dev.off()
            return()
          }
          
          else {
            cat("No if statement entered\n")
          }
          
        },
        
        # -- message function -- #
        message = function(m) {
          timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          shinyjs::html(id = "analysis_log", html = paste0(timestamp, " ", m$message, "\n"), add = TRUE)
          invokeRestart("muffleMessage")
        })
    },
    
    error = function(e) {
      shinyjs::html(id = "analysis_log", html = paste0("❌ Error: ", conditionMessage(e)), add = TRUE)
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    },
    
    finally = {
      shinyjs::enable("run_diff")  # re-enable button
    })
    
  })
    
    
    
    # === PDF results === #
    
    # -- Reactive file loading -- #
    diff_mod_time <- reactiveFileReader(
      intervalMillis = 2000,
      session = session,
      filePath = file.path(static_dir, paste0("diff_analysis_", panel, ".pdf")),
      readFunc = function(path) file.info(path)$mtime)
    
    # -- PDF display -- #
    output$diff_analysis_pdf <- renderUI({
      time <- diff_mod_time()  # reactive dependency
      
      filePath = file.path(static_dir, paste0("diff_analysis_", panel, ".pdf"))
      if (!file.exists(filePath)) {
        return(tags$div(style = "color: gray; font-style: italic;",
                        "Please run a differential analysis to see plots."))
      }
      
      rel_web_path <- file.path("flowsom", paste0("diff_analysis_", panel, ".pdf"))
      
      tags$iframe(
        src = sprintf("%s?time=%s", rel_web_path, as.numeric(time)),  # bust cache
        style = "width:100%; height:800px; border:none;"
      )
    })

    
    # === CSV results === #
    
    # -- Reactive file loading -- #
    csv_path <- file.path(static_dir, paste0("diff_analysis_", panel, ".csv"))
    diff_csv_data <- reactiveFileReader(
      intervalMillis = 2000,
      session = session,
      filePath = csv_path,
      readFunc = function(path) {
        if (file.exists(path)) {
          read.csv(path, header = TRUE)
        } else {
          NULL
        }
      }
    )
    
    # -- CSV display -- #
    output$csv_warning <- renderUI({
      if (!file.exists(csv_path)) {
        tags$div(style = "color: gray; font-style: italic;",
                 "Please run a differential analysis to view results.")
      }
    })
    
    output$diff_analysis_csv <- renderDT({
      req(diff_csv_data())  # waits until file exists
      datatable(diff_csv_data(), options = list(pageLength = 10), rownames = FALSE)
    })

    
  })
}
 
    