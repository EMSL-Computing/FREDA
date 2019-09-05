# A script to run FREDA locally

# 1. Change directory to FREDA directory (fill in correct directory):
setwd("/fill/in/path/to/FREDA")


# 2. Install dependencies from CRAN and Github
cran_deps <- c("shiny", "ggplot2", "reshape2", "webshot", "htmlwidgets", "dplyr", "raster", 
               "magick", "purrr", "shinyBS", "shinyjs", "shinyWidgets", "pander", "readr", 
               "plotly", "DT", "shinycssloaders", "devtools")

github_deps <- c("EMSL-Computing/ftmsRanalysis")

installed_pkgs <- rownames(installed.packages())
cran_deps <- setdiff(cran_deps, installed_pkgs)
if (length(cran_deps) > 0) {
  message("Additional packages will be installed from CRAN: ", paste(cran_deps, collapse=", "))
  install.packages(cran_deps)
}

github_deps <- github_deps[!(basename(github_deps) %in% installed_pkgs)]
if (length(github_deps) > 0) {
  message("Additional packages will be installed from Github: ", paste(github_deps, collapse=", "))
  devtools::install_github(github_deps)
}


# 3. Run shiny app
shiny::runApp(".")
