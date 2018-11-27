print("Testing the DOWNLOAD TAB, all tests through the visualize tab must pass first...")
source("sourced_script/visualize_tab_small.R", local = TRUE)

app$setInputs(top_page = "Download")

app$setInputs(download_selection = c("separate", "merged", "group_data"))
app$snapshotDownload("download_processed_data", file = "images.zip")
app$snapshot(items = list(export = "images_out"))