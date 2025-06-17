###############################
# Install necessary packages #
##############################


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
