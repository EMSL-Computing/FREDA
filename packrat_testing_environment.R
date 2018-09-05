## This is code for producing an environment that replicates the process by which FREDA is 
## pushed to a shiny server. Ensure that you have packrat 0.4.8-1; newer versions will not work

##############################################################################################
## STEP 1: INITIALIZE DIRECTORY AND PACKRAT AND INSTALL LIBRARIES


#############################################################
###### First set up the dir to be a folder for the app (must be directory that doesn't previously exist) #######
# SET THE FOLLOWING IN YOUR R CONSOLE #

## path to folder (That doesn't exist) that R should create ##
vdbDir <- "/Users/clab683/Documents/git_repos/FREDA_packrat"
## path to local repositories (where fticRanalysis folder is located ##
localDir="/Users/clab683/Documents/git_repos"
############################################################

dir.create(vdbDir)

# Now initialize directory to be packrat directory 
setwd(vdbDir)

# you must use packrat 0.4.8-1 #
packrat::init() # this will restart R session

# Make sure to do this after R session is restarted
packrat::on()

# Packrat options:
# - packages to not include in local packrat lib (always make sure KEGGdata and MetaCycData are here)
# - don't automatically load the things in external.packages on startup (personal preference)
# - local.repos = directories of package sources (e.g. local clone of fticr_viz repo if you need icRanalysis)
packrat::set_opts(external.packages=c("stringi", "plogr", "BH", "devtools", "roxygen2", "rgdal"),
                  load.external.packages.on.startup=FALSE,
                  local.repos= localDir)

# IMPORTANT: set options to use this repo, otherwise packrat adds the bioconductor repos and then shiny01 tries to connect to them
# Also note "http" not "https"
options(repos=c(CRAN="http://cran.rstudio.com"))

# Install necessary libraries in packrat's lib directory (this DOES NOT copy the source, just installs for use locally)
install.packages(c("shiny", "shinyBS", "ggplot2", "reshape2","webshot","htmlwidgets","dplyr","raster","magick","purrr", "DT", "shinyjs", "shinycssloaders", "datadr")) 

webshot::install_phantomjs()

# Can also install from local source if necessary (make sure to list the parent directory of this in "local.repos" above)
devtools::install_local("/Users/clab683/Documents/git_repos/fticRanalysis/")


##############################################################################################
## STEP 2: Copy all contents of FREDA folder into vdbDir
##############################################################################################

##############################################################################################
## STEP 3: Call runApp and test
##############################################################################################
##############################################################################################
## STEP 4: BUNDLE TRELLISCOPE VDB

# Double check your repos before snapshotting--packrat likes to reset them
options()$repos # should be just "http://cran.rstudio.com"

# Downloads sources for all packages installed
packrat::snapshot(vdbDir)

# Create a tarball to transfer to shiny01
packrat::bundle(vdbDir)

# Take the .tar.gz file that is created in the vdbDir/packrat/bundles directory and 
# upload to shiny01 using an ftp client
