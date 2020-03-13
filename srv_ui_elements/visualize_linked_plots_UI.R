list(
  # Table of plots that have the ability to be linked/compared
  output$lp_plot_table <- DT::renderDataTable({
    linked_plots_table()
    },
    options = list(scrollX = TRUE, columnDefs = list(list(className = 'nowrap_scroll', targets = '_all'))), 
    escape = FALSE, selection = 'multiple'),
  
  # Left linked plot (corresponds to the first row selected)
  output$lp_left <- renderPlotly({
    # browser()
    req(revals$peakData2)
    input$lp_compare_plots
    lp_lastEvent$source
    
    # get the data corresponding to selected points
    # usually we will link plot components through e_meta
    d <- event_data('plotly_selected', source= lp_lastEvent$source)
    #d_out2 <<- d
    
    # big ol' isolate block
    isolate({
      if(!is_empty(d)){
        d <- d[!sapply(d$key, is.null),] # some keys are null, these are the elements we add to the plot to highlight selected points
        
        row1 <- input$lp_plot_table_rows_selected[1]
        row2 <- input$lp_plot_table_rows_selected[2]
        
        req(all(!is.null(row1), !is.null(row2)))
        
        ## GET VARIABLE RESOURCES FOR DRAWING PLOTS/CONSTRUCTING DATA##
        
        # We need:
        
        # Plot type of the plot that was interacted with, since this determines the structure of 'd', AND ...
        # ... type of the plot that is currently being draw, since this determines how we should add extra elements
        # These two will be the same if we interacted with the plot currently being drawn
        # x-variable of the interacted-with plot and the current plot, usually for if we are dealing with data from/for a histogram
        if(lp_lastEvent$source == 'left_source'){
          ptype_selected = linked_plots_table()[row1, 'Plot Type']
          xvar_selected = linked_plots_table()[row1, 'X Variable']
        }
        else if(lp_lastEvent$source == 'right_source'){
          ptype_selected = linked_plots_table()[row2, 'Plot Type']
          xvar_selected = linked_plots_table()[row2, 'X Variable']
        }
        
        xvar_current = linked_plots_table()[row1, 'X Variable']
        ptype_current = linked_plots_table()[row1, 'Plot Type']
        
        # The samples that are contained in each plot, mostly for purposes of filtering out observations that dont appear in these samples (edata_inds below...)
        sampnames = revals$peakData2$f_data[,getFDataColName(revals$peakData2)]
        g1_samples = linked_plots_table()[row1, 'Group 1 Samples'] %>% stringr::str_extract_all(paste(sampnames, collapse="|")) %>% purrr::pluck(1)
        g2_samples = linked_plots_table()[row1, 'Group 2 Samples'] %>% stringr::str_extract_all(paste(sampnames, collapse="|")) %>% purrr::pluck(1)
        
        edata_inds = revals$peakData2$e_data %>% select(g1_samples) %>% rowSums() %>% {. != 0}
        
        ##
        
        #### CONSTRUCT E-META CORRESPONDING TO SELECTED DATA #### 
        
        # These all depend on the edata id field
        scatter_types = c('Van Krevelen Plot', 'Kendrick Plot', 'Custom Scatter Plot')
        
        # if we selected a scatter plot, the d[['key']] will hold selected ids
        if(ptype_selected %in% scatter_types){
          tmp_dat <- dplyr::filter(revals$peakData2$e_meta, !!rlang::sym(getEDataColName(revals$peakData2)) %in% d[["key"]])
        }
        else if(ptype_selected %in% c('Density Plot')){
          # if we selected a histogram, d$key has the ranges of the selected bins, 
          # sapply:  for each range, get a vector that indicates which elements of the xvar column were in this range
          # apply:  combine all vectors into one vector that is true if any of the vectors are true
          emeta_inds = sapply(d$key, function(x) between(revals$peakData2$e_meta[[xvar_selected]], as.numeric(x[1]), as.numeric(x[2]))) %>% apply(1, any)
          
          # filter down to observations that fell within selected histogram bins AND have at least one observation in the selected samples
          tmp_dat <- revals$peakData2$e_meta[emeta_inds & edata_inds,]
        }
        
        #### UPDATE PLOTS BASED ON TYPE ####
        
        # c('Van Krevelen Plot', 'Kendrick Plot', 'Density Plot', 'Custom Scatter Plot', 'PCOA Plot')
        if(ptype_current == "Van Krevelen Plot"){
          plots$linked_plots$left %>% 
            add_markers(x=~get(getOCRatioColName(revals$peakData2)), y=~get(getHCRatioColName(revals$peakData2)), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Kendrick Plot'){
          plots$linked_plots$left %>% 
            add_markers(x=~get(getKendrickMassColName(revals$peakData2)), y=~get(getKendrickDefectColName(revals$peakData2)), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Custom Scatter Plot'){
          # need the y variable as well for custom scatter plots, since it is not fixed
          yvar = linked_plots_table()[row1, 'Y Variable']
          
          plots$linked_plots$left %>% 
            add_markers(x=~get(xvar_current), y=~get(yvar), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Density Plot'){
          # plotly objects with histogram traces have a 'hist_data' attribute containing bins, bindwidths, etc.
          hist_dat <- attr(plots$linked_plots$left, 'hist_data')
          
          # for each range, get the counts of FILTERED data that fall in that range
          counts <- sapply(hist_dat$key, function(x) nrow(tmp_dat %>% filter(between(!!rlang::sym(xvar_current), x[1], x[2]))))
          
          # normalize by the number of rows in the original data that had nonmissing observations in the current plots displayed samples, as well as the bin width
          density <- counts/sum(edata_inds)/hist_dat$barwidth
          
          hist_dat$counts <- counts
          hist_dat$density <- density
          
          plots$linked_plots$left %>% add_bars(x=~x, y=~density, width=~barwidth, marker = list(color='cyan'), data = hist_dat)
        }
      }
      else{
        plots$linked_plots$left
      }
    })
  }),
  
  # Right linked plot, does the same as above, except the 'current plot' is referenced by the SECOND row selected
  output$lp_right <- renderPlotly({
    #browser()
    req(revals$peakData2)
    input$lp_compare_plots
    lp_lastEvent$source
    
    d <- event_data('plotly_selected', source= lp_lastEvent$source)
    #d_out2 <<- d
    
    isolate({
      if(!is_empty(d)){
        d <- d[!sapply(d$key, is.null),]
        row1 <- input$lp_plot_table_rows_selected[1]
        row2 <- input$lp_plot_table_rows_selected[2]
        
        req(all(!is.null(row1), !is.null(row2)))
        
        xvar_current = linked_plots_table()[row2, 'X Variable']
        ptype_current = linked_plots_table()[row2, 'Plot Type']
        sampnames = revals$peakData2$f_data[,getFDataColName(revals$peakData2)]
        g1_samples = linked_plots_table()[row2, 'Group 1 Samples'] %>% stringr::str_extract_all(paste(sampnames, collapse="|")) %>% purrr::pluck(1)
        g2_samples = linked_plots_table()[row2, 'Group 2 Samples'] %>% stringr::str_extract_all(paste(sampnames, collapse="|")) %>% purrr::pluck(1)
        edata_inds = revals$peakData2$e_data %>% select(g1_samples) %>% rowSums() %>% {. != 0}
        
        if(lp_lastEvent$source == 'left_source'){
          ptype_selected = linked_plots_table()[row1, 'Plot Type']
          xvar_selected = linked_plots_table()[row1, 'X Variable']
        }
        else if(lp_lastEvent$source == 'right_source'){
          ptype_selected = linked_plots_table()[row2, 'Plot Type']
          xvar_selected = linked_plots_table()[row2, 'X Variable']
        }
        
        scatter_types = c('Van Krevelen Plot', 'Kendrick Plot', 'Custom Scatter Plot')
        
        if(ptype_selected %in% scatter_types){
          tmp_dat <- dplyr::filter(revals$peakData2$e_meta, !!rlang::sym(getEDataColName(revals$peakData2)) %in% d[["key"]])
        }
        else if(ptype_selected %in% c('Density Plot')){
          emeta_inds = sapply(d$key, function(x) between(revals$peakData2$e_meta[[xvar_selected]], as.numeric(x[1]), as.numeric(x[2]))) %>% apply(1, any)
          tmp_dat <- revals$peakData2$e_meta[emeta_inds & edata_inds,]
        }
      
        if(ptype_current == "Van Krevelen Plot"){
          plots$linked_plots$right %>% 
            add_markers(x=~get(getOCRatioColName(revals$peakData2)), y=~get(getHCRatioColName(revals$peakData2)), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Kendrick Plot'){
          plots$linked_plots$right %>% 
            add_markers(x=~get(getKendrickMassColName(revals$peakData2)), y=~get(getKendrickDefectColName(revals$peakData2)), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Custom Scatter Plot'){
          yvar = linked_plots_table()[row2, 'Y Variable']
        
          plots$linked_plots$right %>% 
            add_markers(x=~get(xvar_current), y=~get(yvar), data=tmp_dat, 
                        marker=list(color="cyan"), name="Selected", inherit = F)
        }
        else if(ptype_current == 'Density Plot'){
          hist_dat <- attr(plots$linked_plots$right, 'hist_data')
          
          counts <- sapply(hist_dat$key, function(x) nrow(tmp_dat %>% filter(between(!!rlang::sym(xvar_current), x[1], x[2]))))
          
          density <- counts/sum(edata_inds)/hist_dat$barwidth
          
          hist_dat$counts <- counts
          hist_dat$density <- density
          
          plots$linked_plots$right %>% add_bars(x=~x, y=~density, width=~barwidth, marker = list(color='cyan'), data = hist_dat)
        }
      }
      else{
        plots$linked_plots$right
      }
    })
  })
)
