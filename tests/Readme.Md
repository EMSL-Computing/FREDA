TESTING INSTRUCTIONS FOR FREDA 

TLDR:  
library(shinytest)

setwd(location-of-FREDA)

testApp(".", "name of test")

#where "name of test" is the name of one of the scripts in FREDA/tests.  NOT one of the files in FREDA/tests/sourced_script

_______

The tests/ folder in the project root directory contain all scripts to perform testing.

Testing for FREDA is done on a tab-by-tab basis.  Scripts in tests/ call other scripts contained in  the subdirectory tests/sourced_script/.  The files therein perform the invisible navigation of the app and take snapshots, run testthat tests, and export images. 

Most of the test names have the form test_TABNAME_FILESIZE with TABNAME = upload, preprocess, filter, visualize, download and FILESIZE = large or small

To test, say, the filter tab on the small example dataset, call testApp(“.”, “test_filter_small”).  This will call the shinytest code inside “tests/sourced_script/filter_tab_small.R”

It should be noted that this process is cascading.  The app always starts at the upload tab, so it must first navigate through everything to get to the tab you want to test, i.e. when testing the filter tab shinytest will first navigate through the upload and preprocess tabs (and perform tests/take snapshots/save images along the way).  Specifically, one calls testApp(“.”, “test_filter_small”). Which calls “filter_tab_small.R”.  “filter_tab_small.R”, before running any of its own code, calls “preprocess_tab_small.R”, which itself calls “upload_tab_small.R” before running its code.  

Image screenshots are always saved to the tests/screenshots folder and screenshots are overridden every time a test is run to reduce folder clutter.  Additionally, if you test the download tab, it will include a zip file of the downloaded images in its results (if the app worked correctly).

I’ve tried to add print messages to give the tester so.e idea of what is happening during testing…

***under construction***