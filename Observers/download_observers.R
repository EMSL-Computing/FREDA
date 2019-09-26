# observer which write files to temp directory in preparation for download
# this needs to be outside the downloadhandler to get around the server timing out on long downloads
observeEvent(input$makezipfile,{
  disable('makezipfile')
  on.exit({
    enable('makezipfile')
  })
  
  print(tempdir())
  fs <- vector()
  
  # ____test functionality____
  # if in testmode just select all rows since shinytest doesn't recognize row selection for parmsTable2
  if (isTRUE(getOption("shiny.testmode"))){
    rows <- 1:nrow(parmTable$parms)
  }
  else rows <- input$parmsTable2_rows_selected
  #
  total_files <- sum(c(input$report_selection, length(input$download_selection), length(rows))) + 1
  
  withProgress(message = "Writing files: ",{
    # option to choose report output format?  need to change inputs in report.R.
    if (input$report_selection == TRUE & !is.null(revals$peakData2)){
      fs <- c(fs, file.path(tempdir(), "report.html"))
      report(revals$uploaded_data, revals$peakData2, Emeta(), output_file = file.path(tempdir(), "report.html"), output_format = "html_document", 
             C13_ID = input$iso_symbol, groups_list = revals$groups_list)
      incProgress(1/total_files, detail = 'HTML report done..')
    }
    
    # kegg tables
    if (input$download_mappings) {
      for(name in names(tables$mapping_tables)){
        fs <- c(fs, file.path(tempdir(), paste0(name,'.csv')))
        
        # must convert list columns to character
        table_out <- tables$mapping_tables[[name]] %>% mutate_if(is.list, as.character)
        
        write_csv(table_out, path = file.path(tempdir(), paste0(name,'.csv')))
      }
      # 
      rm(table_out)
    }
    
    if ("separate" %in% input$download_selection & !is.null(revals$peakData2)){
      fs <- c(fs, file.path(tempdir(), "FREDA_processed_e_data.csv"), file.path(tempdir(), "FREDA_processed_e_meta.csv"))
      write_csv(revals$peakData2$e_data, path = file.path(tempdir(), "FREDA_processed_e_data.csv"))
      write_csv(revals$peakData2$e_meta, path = file.path(tempdir(), "FREDA_processed_e_meta.csv"))
      incProgress(1/total_files, detail = 'Data and molecular identification file done..')
    }
    if ("merged" %in% input$download_selection & !is.null(revals$peakData2)){
      fs <- c(fs, file.path(tempdir(), "FREDA_processed_merged_data.csv"))
      merged_data <- merge(revals$peakData2$e_data, revals$peakData2$e_meta)
      write_csv(merged_data, path = file.path(tempdir(), "FREDA_processed_merged_data.csv"))
      rm(merged_data)
      incProgress(1/total_files, detail = 'Merged file done..')
    }
    if ("group_data" %in% input$download_selection){
      if(length(revals$plot_data) != 0){
        for(i in 1:length(revals$plot_data)){
          if (!is.null(revals$plot_data[[i]])){
            path <- file.path(tempdir(), paste0("FREDA_group_data_summary_", gsub("/", "-", parmTable$parms[["File Name"]][i]),".csv"))
            fs <- c(fs, path)
            write_csv(revals$plot_data[[i]], path = path) 
          }
        }
      }
      incProgress(1/total_files, detail = 'Group plot summaries done..')
    }
    
    if (length(rows) > 0) {
      bitmaps <- list()
      for (i in rows) {
        path <- paste(tempdir(), "/",gsub("/", "-", parmTable$parms[["File Name"]][i]), ".", input$image_format, sep = "") #create a plot name
        fs <- c(fs, path) # append the new plot to the old plots
        export(revals$plot_list[[i]],
               file = path, zoom = ifelse(peakData2_dim() < max_cells, 2, 1)) # use webshot to export a screenshot to the opened pdf
        #r <- brick(file.path(getwd(), paste("plot",i,".png", sep = ""))) # create a raster of the screenshot
        # img <- magick::image_read(paste(tempdir(), "plot",i,".png", sep = ""))#attr(r,"file")@name) #turn the raster into an image of selected format
        
        # if (isTRUE(getOption("shiny.testmode"))) bitmaps[[i]] <- as.raster(img)
        
        #image_write(img, path = path, format = input$image_format) #write the image
        incProgress(1/total_files, detail = sprintf('Plot %s done..', i))
        #rsvg::rsvg_svg(img, file = path)
      }
      
      # ___test-export___
      exportTestValues(images_out = digest::digest(bitmaps))
      
      fs <- c(fs, file.path(tempdir(), "Plot_key.csv"))
      outtable <- parmTable$parms[rows,]
      write_csv( outtable, path = file.path(tempdir(), "Plot_key.csv"))
    }
    print(fs)
    revals$fs <- fs
    
    # setwd(orig_wd)
  })
})

# Check that files actually got written to the appropriate locations.  If not, do not allow download
observeEvent(revals$fs, {
  if(length(revals$fs) > 0){
    download_condition = sum(file.exists(revals$fs)) > 0
  }
  else download_condition = FALSE
    
  toggleState("download_processed_data", condition = download_condition)
  
}, ignoreNULL = FALSE)
