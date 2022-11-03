for (fname in Sys.glob("tab_navigation/*")) {
  source(fname)
}

# Load application support files into testing environment
shinytest2::load_app_env()
