###########################
# Globally available data #
###########################


#### IMPORTS ####

library(shiny)
library(ggplot2)
library(dplyr)
library(FlowSOM)


#### LOADING DATA ####

# === Cell counts csv for gating_image_module === #
cell_counts <- read.csv("data/cell_count.csv")

cell_count_data <- reactive({
  cell_counts 
})


# === Cofactors csv for transformation_image_module === #
cofactors <- read.csv("data/cofactors.csv")

cofactor_data <- reactive({
  cofactors 
})


# === PeacoQC results csv for qc_image_module === #
peacoQC <- read.csv("data/peacoQC.csv")

peacoQC_data <- reactive({
  peacoQC 
})


# === Marker ranges csv for normalization_image_module === #
marker_range <- read.csv("data/marker_range.csv")

marker_range_data <- reactive({
  marker_range 
})


# === FlowSOM model paths for all FlowSOM sections === #
# model path
get_model_path <- function(panel) {
  file.path("data", panel, paste0("fsom_model_", panel, ".rds"))
}

# check model existence for warnings
model_exists <- function(panel) {
  file.exists(get_model_path(panel))
}

