#library(ProntoPCR)
# Load required packages --------------------------------------------------
library(shiny)
library(shinyBS)
library(fontawesome)
library(rmarkdown)
library(readr)
library(DT)
library(tidyverse)
library(magrittr)
library(datawizard)
library(purrr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(bslib)
library(ggtext)
library(stringr)
library(shinyjs)
library(car)
library(multcomp)
library(dplyr)
library(rlang)
library(tidyr)
library(broom)
library(DescTools)
library(FSA)
library(rstatix)
library(multcompView)
library(Cairo)
library(remotes)
library(showtext)
library(sysfonts)
library(svglite)
library(kableExtra)

# Source modules for shinyapps --------------------------------------------------
# Dynamically source all module files in the R/ directory
module_files <- list.files("R", pattern = "^module_.*\\.R$", full.names = TRUE)
sapply(module_files, source, local = TRUE)

if (!requireNamespace("ProntoPCR", quietly = TRUE)) {
  devtools::install_github("MarnieMaddock/ProntoPCR")
}

# Run the app
ProntoPCR()



