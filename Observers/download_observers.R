# observer which write files to temp directory in preparation for download
# this needs to be outside the downloadhandler to get around the server timing out on long downloads
observeEvent(input$makezipfile, {
  disable('makezipfile')
  on.exit({
    enable('makezipfile')
  })

  print(tempdir())
  fs <- vector()

  #
  total_files <- sum(c(input$report_selection, input$download_mappings, length(input$download_selection), length(plots$plot_table))) + 1

  plots_marked_for_death <- which(plots$plot_table[, 2] == dt_checkmark)

  withProgress(message = "Writing files: ", {
    # option to choose report output format?  need to change inputs in report.R.
    if (input$report_selection == TRUE & !is.null(revals$peakData2)) {
      tryCatch({
        fs <- c(fs, file.path(tempdir(), "report.html"))
        report(revals$uploaded_data, revals$peakData2, revals$uploaded_data$e_meta, output_file = file.path(tempdir(), "report.html"), output_format = "html_document",
          C13_ID = input$iso_symbol, groups_list = revals$groups_list, db_tables_info = tables$saved_db_info)
      },
      error = function(e) {
        msg = paste0('Error creating report: \n System error: ', e)
        revals$warningmessage_download$report_error <<- msg
        msg
      })
      incProgress(1 / total_files, detail = 'HTML report done..')
    }

    # kegg tables
    if (input$download_mappings) {
      for (name in names(tables$mapping_tables)) {
        fs <- c(fs, file.path(tempdir(), paste0(name, '.csv')))

        # must convert list columns to character
        table_out <- tables$mapping_tables[[name]] %>% mutate_if(is.list, as.character)
        write_csv(table_out, path = file.path(tempdir(), paste0(name, '.csv')))
        incProgress(1 / total_files, detail = sprintf('%s done..', name))
      }
      #
      rm(table_out)
    }

    if ("separate" %in% input$download_selection & !is.null(revals$peakData2)) {
      fs <- c(fs, file.path(tempdir(), "FREDA_processed_e_data.csv"), file.path(tempdir(), "FREDA_processed_e_meta.csv"))
      write_csv(revals$peakData2$e_data, path = file.path(tempdir(), "FREDA_processed_e_data.csv"))
      write_csv(revals$peakData2$e_meta, path = file.path(tempdir(), "FREDA_processed_e_meta.csv"))
      incProgress(1 / total_files, detail = 'Data and molecular identification file done..')
    }
    if ("merged" %in% input$download_selection & !is.null(revals$peakData2)) {
      fs <- c(fs, file.path(tempdir(), "FREDA_processed_merged_data.csv"))
      merged_data <- merge(revals$peakData2$e_data, revals$peakData2$e_meta)
      write_csv(merged_data, path = file.path(tempdir(), "FREDA_processed_merged_data.csv"))
      rm(merged_data)
      incProgress(1 / total_files, detail = 'Merged file done..')
    }
    if ("group_data" %in% input$download_selection) {
      if (length(plots$plot_data) != 0) {
        for (name in names(plots$plot_data)) {
          path <- file.path(tempdir(), paste0("FREDA_group_data_summary_", gsub("/", "-", name), ".csv"))
          fs <- c(fs, path)
          write_csv(plots$plot_data[[name]], path = path)
        }
      }
      incProgress(1 / total_files, detail = 'Group plot summaries done..')
    }

    if (length(plots_marked_for_death) > 0) {
      # fix stupid, enforce maximum height/width
      width = if (is_empty(input$download_img_width)) 1600 else input$download_img_width
      height = if (is_empty(input$download_img_height)) 900 else input$download_img_height

      if (isTRUE(input$download_img_width > 2400)) {
        width = 2400
      }

      if (isTRUE(input$download_img_height > 2400)) {
        height = 2400
      }
      #

      for (i in plots_marked_for_death) {
        plot_key = plots$plot_table[i, 1]
        filename = paste0(plot_key, '.', input$image_format)
        path = file.path(tempdir(), filename)
        fs <- c(fs, path)

        if (inherits(plots$plot_list[[plot_key]], 'plotly')) {
          # using withr until orca handles full paths
          withr::with_dir(tempdir(), orca(plots$plot_list[[plot_key]], filename, width = width, height = height, scale = 2))
          # export(plots$plot_list[[plot_key]], file = path, zoom = 2) # deprecated for some ungodly reason
        }
        else if (inherits(plots$plot_list[[plot_key]], 'ggplot')) {
          # TODO: variable dpi for ggplots
          ggsave(path, plots$plot_list[[plot_key]], width = width / 300, height = height / 300, units = 'in')
        }
        incProgress(1 / total_files, detail = sprintf('Plot: %s done..', plot_key))
      }
    }

    print(fs)
    revals$fs <- fs
  })
})

# Check that files actually got written to the appropriate locations.  If not, do not allow download
observeEvent(revals$fs, {
  if (length(revals$fs) > 0) {
    download_condition = sum(file.exists(revals$fs)) > 0
  }
  else download_condition = FALSE

  toggleState("download_processed_data", condition = download_condition)

}, ignoreNULL = FALSE)

### Plots

# remove or add a plot from the download queue
observeEvent(input$mark_plot_download, {
  req(length(input$download_plot_table_rows_selected) > 0)
  cond = plots$plot_table[input$download_plot_table_rows_selected, 2] == dt_minus

  if (cond) {
    plots$plot_table[input$download_plot_table_rows_selected, 2] <- dt_checkmark
  }
  else {
    plots$plot_table[input$download_plot_table_rows_selected, 2] <- dt_minus
  }
})

# remove the selected plot on button click
# need to remove the entry plots$plot_table and the corresponding plot in plots$allplots
observeEvent(input$remove_plot_download, {
  req(length(input$download_plot_table_rows_selected) > 0)
  plot_name = plots$plot_table[input$download_plot_table_rows_selected, 1]

  plots$plot_table <- plots$plot_table %>% filter(`File Name` != plot_name)
  plots$plot_list[[plot_name]] <- NULL
  plots$plot_data[[plot_name]] <- NULL
})

###

# store separate table for the download page because js is picky
observeEvent(plots$plot_table, {
  plots$plot_table_download <- plots$plot_table
})

# enforce height/width limits
observeEvent(c(input$download_img_width, input$download_img_height), {
  revals$warningmessage_download$excess_width <- revals$warningmessage_download$excess_height <- NULL
  if (isTRUE(input$download_img_width > 2400)) {
    revals$warningmessage_download$excess_width <- 'style = color:red>Max width is 2400, value will be truncated.'
    # updateNumericInput(session, 'download_img_width', value = 2400)
  }
  if (isTRUE(input$download_img_height > 2400)) {
    revals$warningmessage_download$excess_height <- 'style = color:red>Max height is 2400, value will be truncated.'
    # updateNumericInput(session, 'download_img_height', value = 2400)
  }
})
