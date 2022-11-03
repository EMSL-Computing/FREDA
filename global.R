enableBookmarking(store = 'server')

library(shiny)
library(ftmsRanalysis)
library(ggplot2)
library(reshape2)
library(webshot)
library(htmlwidgets)
library(dplyr)
library(markdown)
library(raster)
library(purrr)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(pander)
library(readr)
library(plotly)
library(DT)
library(shinycssloaders)

# uncomment either library() or load_all() if you need to load kegg
library(KeggData)
library(MetaCycData)
# devtools::load_all('~/Documents/git_repos/MetaCycData/')
# devtools::load_all('~/Documents/git_repos/KeggData/')

# static variables #
dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'

dbe_opts_info <- 'Semicolon separated strings of elements and their valences, i.e. to calculate the dbe for two sets of valences, input C4H1N3O2S2P3;C3H3N2O2S4P4'
kendrick_opts_info <- 'The base compound(s) used to calculate the Kendrick Mass.  See <a href = ""https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5447562/"">[Foquet and Sato, 2017]</a> for details.'

dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'

ttip_text = list(
  "plot_save"="Save the last created plot", 
  "plot_review"="Review saved plots", 
  "page_help"="How do I use this page?"
)

#------ Download Example Data ---------#
example_edata <- read_csv('Data/example12T_edata.csv') %>% as.data.frame(stringsAsFactors = FALSE)
example_emeta <- read_csv('Data/example12T_emeta.csv') %>% as.data.frame(stringsAsFactors = FALSE)
calc_opts <- read_csv('calculation_options.csv') %>% as.data.frame(stringsAsFactors = FALSE)
calc_vars <- read_csv('calculation_variables.csv') %>% as.data.frame(stringsAsFactors = FALSE)
# determines when 'large data' options are triggered
max_cells <- 2000000

PREPROCESS_CALC_VARS <- list(
  "ColumnName" = "DisplayName",
  "OtoC_ratio" = "O:C Ratio",
  "HtoC_ratio" = "H:C Ratio",
  "NtoC_ratio" = "N:C Ratio",
  "PtoC_ratio" = "P:C Ratio",
  "NtoP_ratio" = "N:P Ratio",
  "NOSC" = "NOSC",
  "GFE" = "Gibbs Free Energy",
  "AI" = "Aromaticity",
  "AI_Mod" = "Modified Aromaticity",
  "DBE" = "DBE",
  "DBE_O" = "DBE - O",
  "DBE_AI" = "DBE - AI",
  "ElComposition" = "Elemental Composition",
  "bs1_class" = "Compound Classes Using bs1",
  "bs2_class" = "Compound Classes Using bs2",
  "bs3_class" = "Compound Classes Using bs3"
) 

BASE_COMPS <- c("CH2", "CO2", "H2", "H2O", "CHO")

.extra_kmass <- lapply(BASE_COMPS, function(x) sprintf("Kendrick Mass (Base %s)", x))
names(.extra_kmass) <- paste0("kmass.", BASE_COMPS)

.extra_kdefect <- lapply(BASE_COMPS, function(x) sprintf("Kendrick Defect (Base %s)", x))
names(.extra_kdefect) <- paste0("kdefect.", BASE_COMPS)

PREPROCESS_CALC_VARS <- c(PREPROCESS_CALC_VARS, .extra_kmass, .extra_kdefect)

# Info text for things such as warnings
info_text = list(
  COREMS_UPLOAD_ERROR = "There was an error retrieving your Core-MS files: %s",
  COREMS_UPLOAD_NOSAMPS = "No files found in the cloud location specified by 'corems-prefix' in the URL.",
  COREMS_UPLOAD_SUCCESS = "The following files were uploaded from Core-MS: <br>",
  VALID_LINKED_PLOTS = "Current valid plots to link are Van-Krevelen, Kendrick, single sample density, and custom scatter plots."
)

# cloud/minio resources
VALID_MINIO_HEADER_PARAMS = c("corems-prefix")
