##############################################################
# Install necessary packages and download + verify fcs files #
##############################################################


#### NECESSARY PACKAGES ####

cat("Downloading necessary packages\n")

# BiocManager for Bioconductor packages
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# CRAN packages
cran_packages <- c(
  "shiny", "bslib", "bsicons", "here", "shinycssloaders",
  "future", "furrr", "ggplot2", "ggpubr", "shinyjs", 
  "stringr", "DT"
)

# Bioconductor packages
bioc_packages <- c("FlowSOM", "flowCore")


# Install missing CRAN packages
missing_cran <- cran_packages[!(cran_packages %in% installed.packages()[, "Package"])]
if (length(missing_cran)) {
  install.packages(missing_cran)
}


# Install missing Bioconductor packages
missing_bioc <- bioc_packages[!(bioc_packages %in% installed.packages()[, "Package"])]
if (length(missing_bioc)) {
  BiocManager::install(missing_bioc)
}

# Optionally load all libraries to confirm install
all_packages <- c(cran_packages, bioc_packages)
invisible(lapply(all_packages, library, character.only = TRUE))



#### DATA FOLDERS ####

cat("Data download, cleanup and verification\n")

# Expected number of FCS files per folder
expected_fcs_counts <- list(
  "data/intracell/peanut"  = 108,
  "data/intracell/unstim"  = 91,
  "data/surface/peanut"    = 95,
  "data/surface/unstim"    = 94
)

# Check each folder
for (folder in names(expected_fcs_counts)) {
  
  # Remove .gitkeep if present
  gitkeep_path <- file.path(folder, ".gitkeep")
  if (file.exists(gitkeep_path)) {
    file.remove(gitkeep_path)
    cat(paste("Removed:", gitkeep_path, "\n"))
  }
  
  # List all files in the folder
  files <- list.files(folder, full.names = TRUE)
  
  # Validate only .fcs files are present
  non_fcs_files <- files[!grepl("\\.fcs$", files, ignore.case = TRUE)]
  if (length(non_fcs_files) > 0) {
    stop(paste("ERROR: Non-FCS files found in", folder, ":\n", paste(non_fcs_files, collapse = "\n")))
  }
  
  # Validate number of .fcs files
  fcs_count <- length(files)
  expected_count <- expected_fcs_counts[[folder]]
  if (fcs_count != expected_count) {
    stop(paste("ERROR: Expected", expected_count, "FCS files in", folder, "but found", fcs_count))
  } else {
    cat(paste("SUCESS", folder, "contains", fcs_count, "FCS files as expected.\n"))
  }
}

cat("SUCESS: all folders validated.\n")

