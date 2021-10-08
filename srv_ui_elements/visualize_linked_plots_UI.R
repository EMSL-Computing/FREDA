list(
  # Table of plots that have the ability to be linked/compared
  output$lp_plot_table <- DT::renderDataTable({
    linked_plots_table()
    },
    options = list(scrollX = TRUE, columnDefs = list(list(className = 'nowrap_scroll', targets = '_all'))), 
    escape = FALSE, selection = 'multiple'),
  
  # Left linked plot (corresponds to the first row selected)
  output$lp_left <- renderPlotly({
    req(revals$peakData2)
    input$lp_compare_plots
    lp_lastEvent$trigger
    
    # big ol' isolate block
    isolate({
      # get the data corresponding to selected points
      # usually we will link plot components through e_meta
      d <- event_data('plotly_selected', source= lp_lastEvent$source)
      
      # must have two selected rows
      row1 <- input$lp_plot_table_rows_selected[1]
      row2 <- input$lp_plot_table_rows_selected[2]
      req(all(!is.null(row1), !is.null(row2)))
      
      ## GET VARIABLE RESOURCES FOR DRAWING PLOTS/CONSTRUCTING DATA##
      
      # We need:
      
      #' 1. Plot type of the plot that was interacted with, since this determines 
      #' the structure of 'd'.
      #' 
      #' 2. Type of the plot that is currently being drawn, since this determines 
      #' how we should add extra elements
      #' 
      #' 1/2 Will be the same if we interacted with the plot currently 
      #' being drawn.
      #' 
      #' x-variable of the interacted-with plot and the current plot, usually 
      #' for if we are dealing with data from/for a histogram
      
      pname_current = linked_plots_table()[row1, 'File Name']
      xvar_current = linked_plots_table()[row1, 'X Variable']
      ptype_current = linked_plots_table()[row1, 'Plot Type']
      
      #' The samples that are contained in each plot, mostly for purposes of 
      #' filtering out observations that dont appear in these samples 
      #' (edata_inds below...)
      sampnames = revals$peakData2$f_data[,getFDataColName(revals$peakData2)]
      g1_samples = linked_plots_table()[row1, 'Group 1 Samples'] %>% 
        stringr::str_extract_all(paste(sampnames, collapse="|")) %>% 
        purrr::pluck(1)
      g2_samples = linked_plots_table()[row1, 'Group 2 Samples'] %>% 
        stringr::str_extract_all(paste(sampnames, collapse="|")) %>% 
        purrr::pluck(1)
      
      #' indices of e_data for all the peaks that have at least one nonzero
      #' observation for all the samples for this plot.
      edata_inds = revals$peakData2$e_data %>% 
        select(getEDataColName(revals$peakData2)) %>% 
        mutate(`__INCLUDE_EDATA__` = revals$peakData2$e_data %>%
                 select(g1_samples) %>%
                 rowSums(na.rm = T) %>%
                 {. != 0} 
        )
      ##
      
      if(!is_empty(d)){
        d <- d[!sapply(d$key, is.null),] # some keys are null, these are the elements we add to the plot to highlight selected points
        
        # plot type and x variable for -selected- plot
        if(lp_lastEvent$source == 'left_source'){
          ptype_selected = linked_plots_table()[row1, 'Plot Type']
          xvar_selected = linked_plots_table()[row1, 'X Variable']
        }
        else if(lp_lastEvent$source == 'right_source'){
          ptype_selected = linked_plots_table()[row2, 'Plot Type']
          xvar_selected = linked_plots_table()[row2, 'X Variable']
        }
        
        #### CONSTRUCT E-META CORRESPONDING TO SELECTED DATA #### 
        
        # These all depend on the edata id field
        scatter_types = c('Van Krevelen Plot', 'Kendrick Plot', 'Custom Scatter Plot')
        
        # if we selected a scatter plot, the d[['key']] will hold selected ids
        if(ptype_selected %in% scatter_types){
          tmp_dat <- dplyr::filter(revals$peakData2$e_meta, !!rlang::sym(getEDataColName(revals$peakData2)) %in% d[["key"]])
        }
        else if(ptype_selected %in% c('Density Plot')){
          #' A dataframe that indicates if certain values of the variable in
          #' question lie inside any of the bins selected on the histogram.
          emeta_inds <- revals$peakData2$e_meta %>% 
            select(getEDataColName(revals$peakData2)) %>% 
            mutate(`__INCLUDE_EMETA__` = sapply(d$key, function(x) {
                # histogram data includes values on the right edge, not the left
                revals$peakData2$e_meta[[xvar_selected]] > as.numeric(x[1]) &
                revals$peakData2$e_meta[[xvar_selected]] <= as.numeric(x[2])
              }) %>%
              apply(1, any)
            )
          
          #' filter down to observations that fell within selected histogram 
          #' bins AND have at least one observation in the selected samples
          selected_rows <- emeta_inds %>%  
            left_join(edata_inds) %>% 
            select(-one_of(getEDataColName(revals$peakData2))) %>% 
            apply(1, all)
          
          tmp_dat <- revals$peakData2$e_meta[selected_rows,]
        }
        
        #### UPDATE PLOTS BASED ON TYPE ####
        
        # c('Van Krevelen Plot', 'Kendrick Plot', 'Density Plot', 'Custom Scatter Plot', 'PCOA Plot')
        if(ptype_current == "Van Krevelen Plot"){
          p <- plots$linked_plots$left %>% 
            add_markers(x=~get(getOCRatioColName(revals$peakData2)), y=~get(getHCRatioColName(revals$peakData2)), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Kendrick Plot'){
          p <- plots$linked_plots$left %>% 
            add_markers(x=~get(getKendrickMassColName(revals$peakData2)), y=~get(getKendrickDefectColName(revals$peakData2)), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Custom Scatter Plot'){
          # need the y variable as well for custom scatter plots, since it is not fixed
          yvar = linked_plots_table()[row1, 'Y Variable']
          
          p <- plots$linked_plots$left %>% 
            add_markers(x=~get(xvar_current), y=~get(yvar), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Density Plot'){
          #' plotly objects with histogram traces have a 'hist_data' attribute 
          #' containing bins, bindwidths, etc.
          hist_dat <- attr(plots$linked_plots$left, 'hist_data')
          
          #' for each range, get the counts of FILTERED data that fall in that range
          counts <- sapply(hist_dat$key, function(x) {
            nrow(
              tmp_dat %>% 
                filter(
                  !!rlang::sym(xvar_current) > x[1] & 
                  !!rlang::sym(xvar_current) <= x[2]
                )
            )
          })
          
          #' normalize by the number of rows in the original data that had 
          #' nonmissing observations in the current plots displayed samples, 
          #' as well as the bin width
          density <- counts/sum(edata_inds$`__INCLUDE_EDATA__`)/hist_dat$barwidth
          
          hist_dat$counts <- counts
          hist_dat$density <- density
          
          p <- plots$linked_plots$left %>% 
            add_bars(x=~x, y=~density, width=~barwidth, marker = list(color='cyan'), data = hist_dat)
        }
      }
      else{
        p <- plots$linked_plots$left
      }
      
      isolate({
        plots$last_plot[[input$top_page]][['left']] <- list(p)
        names(plots$last_plot[[input$top_page]][['left']]) <- pname_current
      })
      
      p
      
    })
  }),
  
  #' Right linked plot, does the same as above, except the 'current plot' is 
  #' referenced by the SECOND row selected.
  output$lp_right <- renderPlotly({
    req(revals$peakData2)
    input$lp_compare_plots
    lp_lastEvent$trigger
    
    isolate({
      d <- event_data('plotly_selected', source= lp_lastEvent$source)
      
      row1 <- input$lp_plot_table_rows_selected[1]
      row2 <- input$lp_plot_table_rows_selected[2]
      req(all(!is.null(row1), !is.null(row2)))
      
      pname_current = linked_plots_table()[row2, 'File Name']
      xvar_current = linked_plots_table()[row2, 'X Variable']
      ptype_current = linked_plots_table()[row2, 'Plot Type']
      sampnames = revals$peakData2$f_data[,getFDataColName(revals$peakData2)]
      g1_samples = linked_plots_table()[row2, 'Group 1 Samples'] %>% 
        stringr::str_extract_all(paste(sampnames, collapse="|")) %>% 
        purrr::pluck(1)
      g2_samples = linked_plots_table()[row2, 'Group 2 Samples'] %>% 
        stringr::str_extract_all(paste(sampnames, collapse="|")) %>% 
        purrr::pluck(1)
      edata_inds = revals$peakData2$e_data %>% 
        select(getEDataColName(revals$peakData2)) %>% 
        mutate(`__INCLUDE_EDATA__` = revals$peakData2$e_data %>%
                 select(g1_samples) %>%
                 rowSums(na.rm = T) %>%
                 {. != 0} 
        )
      
      scatter_types = c('Van Krevelen Plot', 'Kendrick Plot', 'Custom Scatter Plot')
      
      if(!is_empty(d)){
        d <- d[!sapply(d$key, is.null),]
        
        if(lp_lastEvent$source == 'left_source'){
          ptype_selected = linked_plots_table()[row1, 'Plot Type']
          xvar_selected = linked_plots_table()[row1, 'X Variable']
        }
        else if(lp_lastEvent$source == 'right_source'){
          ptype_selected = linked_plots_table()[row2, 'Plot Type']
          xvar_selected = linked_plots_table()[row2, 'X Variable']
        }
        
        if(ptype_selected %in% scatter_types){
          tmp_dat <- dplyr::filter(revals$peakData2$e_meta, !!rlang::sym(getEDataColName(revals$peakData2)) %in% d[["key"]])
        }
        else if(ptype_selected %in% c('Density Plot')){
          emeta_inds <-  revals$peakData2$e_meta %>% 
            select(getEDataColName(revals$peakData2)) %>% 
            mutate(`__INCLUDE_EMETA__` = sapply(d$key, function(x) {
                revals$peakData2$e_meta[[xvar_selected]] > as.numeric(x[1]) &
                revals$peakData2$e_meta[[xvar_selected]] <= as.numeric(x[2])
              }) %>%
              apply(1, any)
            )
          
          selected_rows <- emeta_inds %>%  
            left_join(edata_inds) %>% 
            select(-one_of(getEDataColName(revals$peakData2))) %>% 
            apply(1, all)
          
          tmp_dat <- revals$peakData2$e_meta[selected_rows,]
        }
      
        if(ptype_current == "Van Krevelen Plot"){
          p <- plots$linked_plots$right %>% 
            add_markers(x=~get(getOCRatioColName(revals$peakData2)), y=~get(getHCRatioColName(revals$peakData2)), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Kendrick Plot'){
          p <- plots$linked_plots$right %>% 
            add_markers(x=~get(getKendrickMassColName(revals$peakData2)), y=~get(getKendrickDefectColName(revals$peakData2)), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Custom Scatter Plot'){
          yvar = linked_plots_table()[row2, 'Y Variable']
        
          p <- plots$linked_plots$right %>% 
            add_markers(x=~get(xvar_current), y=~get(yvar), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Density Plot'){
          hist_dat <- attr(plots$linked_plots$right, 'hist_data')
          
          counts <- sapply(hist_dat$key, function(x) {
              nrow(
                tmp_dat %>% 
                  filter(
                    !!rlang::sym(xvar_current) > x[1] & 
                    !!rlang::sym(xvar_current) <= x[2]
                  )
              )
            })
          
          density <- counts/sum(edata_inds$`__INCLUDE_EDATA__`)/hist_dat$barwidth
          
          hist_dat$counts <- counts
          hist_dat$density <- density
          
          p <- plots$linked_plots$right %>% 
            add_bars(x=~x, y=~density, width=~barwidth, marker = list(color='cyan'), data = hist_dat)
        }
      }
      else{
        p <- plots$linked_plots$right
      }
      
      isolate({
        plots$last_plot[[input$top_page]][['right']] <- list(p)
        names(plots$last_plot[[input$top_page]][['right']]) <- pname_current
      })
      
      p
    })
  })
)
