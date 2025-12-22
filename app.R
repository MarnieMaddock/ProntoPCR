# Load required packages --------------------------------------------------
library(shiny)
# # Source modules for shinyapps --------------------------------------------------
# # Dynamically source all module files in the R/ directory
# module_files <- list.files("R", pattern = "^module_.*\\.R$", full.names = TRUE)
# sapply(module_files, source, local = TRUE)
# 
# if (!requireNamespace("ProntoPCR", quietly = TRUE)) {
#   devtools::install_github("MarnieMaddock/ProntoPCR")
# }

# Run the app
ProntoPCR()



