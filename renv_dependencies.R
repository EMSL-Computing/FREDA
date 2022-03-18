#'This script is never actually run, it is for packages that need to be captured
#'in a snapshot() but we dont actually want to call library() for in the app.
#'For example datadr is only suggested by ftmsRanalysis, but is called in the
#'app by one of ftmsRanalysis' functions.
#'
#'Other libraries need to be included so that the correct version installs.  For
#'some reason 'xfun' is not updated to the correct version for rmarkdown.

library(datadr)
library(xfun)
library(kableExtra)
