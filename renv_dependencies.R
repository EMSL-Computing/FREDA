#'This script is never actually run, it is for packages that need to be captured
#'in a snapshot() but we dont actually want to call library() for in the app.
#'For example datadr is only suggested by ftmsRanalysis, but is called in the
#'app by one of ftmsRanalysis' functions.
#'

library(datadr)