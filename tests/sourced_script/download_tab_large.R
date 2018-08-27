print("Testing the DOWNLOAD TAB, all tests through the visualize tab must pass first...")
source("sourced_script/visualize_tab_large.R", local = TRUE)

app$setInputs(top_page = "Download")
app$snapshotDownload("download_processed_data", filename = "images.zip")
app$snapshot(items = list(export = "images_out"))