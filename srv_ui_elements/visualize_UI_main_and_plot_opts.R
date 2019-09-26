list(
  #### Main Panel (Visualize Tab) ####
  # Main plotting output #
  output$FxnPlot <- renderPlotly({
    req(!is.null(input$chooseplots))
    
    # reactive dependencies
    input$update_axes
    input$vk_colors
    input$colorpal
    input$vkbounds
    revals$makeplot #in case vk_colors does not change we still want to redraw the plot.
    
    disable('plot_submit')
    on.exit({
      enable('plot_submit')
      enable("add_plot")
    })
    
    # for testing if plot actually got updated in test mode
    exportTestValues(plot = NULL, plot_attrs = NULL)
    isolate({  
      if (v$clearPlot){
        return(NULL)
      } 
      else {
        # Make sure a plot stype selection has been chosen
        validate(need(input$choose_single != 0, message = "Please select plotting criteria"))
        
        revals$legendTitle = ifelse(is.null(input$legend_title_input) || (input$legend_title_input == ""),
                                    yes = names(revals$color_by_choices[revals$color_by_choices == input$vk_colors]),
                                    no = input$legend_title_input
        )
        
        # Apply custom color scale if numeric is selected
        if (numeric_selected() & !(input$vk_colors %in% c("bs1", "bs2"))){
          diverging_options = c("RdYlGn")
          pal <- RColorBrewer::brewer.pal(n = 9, input$colorpal)
          
          # diverging_options specify color palletes that look weird if they are truncated: [3:9], only truncate the 'normal' ones
          if (!(input$colorpal %in% diverging_options)){
            pal <- RColorBrewer::brewer.pal(n = 9, input$colorpal)[3:9]
          }
          
          # flip the color scale on button click
          if (input$flip_colors %% 2 != 0){
            pal <- rev(pal)
          }
          
          # get domain and obtain color pallette function
          domain = range(plot_data()$e_meta[,input$vk_colors], na.rm = TRUE)
          colorPal <- scales::col_numeric(pal, domain)
          #revals$colorPal <- paste(paste(pal, collapse = ","), paste(domain, collapse = ","), sep = ":")
        }
        else if(!(input$choose_single %in% c(3,4)) & !(input$vk_colors %in% c("bs1", "bs2"))){
          # if there are too many categories, warn user and provide color palette
          if(length(unique(plot_data()$e_meta[, input$vk_colors])) > 12){
            ramp <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
            pal <- ramp(length(unique(plot_data()$e_meta[, input$vk_colors])))
            colorPal <- scales::col_factor(pal, domain = unique(plot_data()$e_meta[, input$vk_colors]))
          }
          else colorPal <- NA
          #revals$colorPal <- NA
        }
        else if(input$choose_single %in% c(3,4)){
          pal = switch(input$colorpal,
                       'default' = c("#7fa453", "#a16db8", "#cb674a"), 'bpr' = c("#0175ee", '#7030A0', "#fd003d"),
                       'neutral' = c("#FC8D59", '#7030A0', "#91CF60"), 'bpg' = c('#8377cb', '#c95798', '#60a862'),
                       'rblkgn' = c('red', 'black', 'green'))
          
          # still allow color_inversion, even though it looks weird
          if (input$flip_colors %% 2 != 0){
            pal <- rev(pal)
          }
          
          pal <- pal[c(1,3,2)] # dont ask
          
          domain <- unique(plot_data()$e_data[,which(grepl('^uniqueness', colnames(plot_data()$e_data)))])
          domain <- domain[which(!is.na(domain))]
          colorPal <- scales::col_factor(pal, domain = domain)
        }
        else colorPal <- NA # just in case....
        
        #----------- Single sample plots ------------#
        #-------Kendrick Plot-----------# 
        if (input$chooseplots == 'Kendrick Plot') {
          validate(need(!is.null(input$whichSamples) | !(is.null(g1_samples()) & is.null(g2_samples())), message = "Please select at least 1 sample"))
          p <- kendrickPlot(plot_data(), colorCName = input$vk_colors, colorPal = colorPal,
                            xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                            title = input$title_input,legendTitle = revals$legendTitle)
          
          if (input$vk_colors %in% c('bs1', 'bs2')) {
            p <- kendrickPlot(plot_data(), vkBoundarySet = input$vk_colors,
                              xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                              title = input$title_input,legendTitle = revals$legendTitle)
          } else {
            # if color selection doesn't belong to a boundary, color by test
            p <- kendrickPlot(plot_data(), colorCName = input$vk_colors, colorPal = colorPal,
                              xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                              title = input$title_input,legendTitle = revals$legendTitle)
          }
        }
        #-------VanKrevelen Plot--------#
        if (input$chooseplots == 'Van Krevelen Plot') {
          validate(need(!is.null(input$whichSamples) | !(is.null(g1_samples()) & is.null(g2_samples())), message = "Please select at least 1 sample"))
          if (input$vkbounds == 0) { #no bounds
            # if no boundary lines, leave the option to color by boundary
            if (input$vk_colors %in% c('bs1', 'bs2')) {
              p <- vanKrevelenPlot(plot_data(), showVKBounds = FALSE, vkBoundarySet = input$vk_colors,
                                   xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                   title = input$title_input,legendTitle = revals$legendTitle)
            } else {
              # if no boundary lines and color selection doesn't belong to a boundary, color by test
              p <- vanKrevelenPlot(plot_data(), showVKBounds = FALSE, colorCName = input$vk_colors, colorPal = colorPal,
                                   xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                   title = input$title_input,legendTitle = revals$legendTitle)
            }
          } else {
            # if boundary lines, allow a color by boundary class 
            if (input$vk_colors %in% c('bs1', 'bs2')) {
              p <- vanKrevelenPlot(plot_data(), vkBoundarySet = input$vkbounds, showVKBounds = TRUE,
                                   xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                   title = input$title_input,legendTitle = revals$legendTitle)
            } else {
              # if boundary lines and color isn't a boundary class
              p <- vanKrevelenPlot(plot_data(), vkBoundarySet = input$vkbounds, showVKBounds = TRUE, 
                                   colorCName = input$vk_colors, colorPal = colorPal,
                                   xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                   title = input$title_input,legendTitle = revals$legendTitle)
            }
          }
        }
        
        #--------- Density Plot --------#
        if (input$chooseplots == 'Density Plot') {
          validate(need(!is.null(input$whichSamples) | !(is.null(g1_samples()) & is.null(g2_samples())), message = "Please select at least 1 sample"),
                   need(!is.na(input$vk_colors), message = "Please select a variable")
          )
          
          # sample/group inputs depending on whether or not we are doing a comparison of groups
          if (input$choose_single %in% c(3,4)){
            samples = FALSE
            groups = unique(attr(plot_data(), "group_DF")$Group)
          }
          else if (input$choose_single == 2){
            samples = input$whichSamples
            groups = "Group"
          }
          else if (input$choose_single == 1){
            samples = input$whichSamples
            groups = FALSE
          }
          
          # if x axis input field is empty, get the display name of the color_by_choices vector index that equals vk_colors, otherwise use what the user typed
          xlabel = ifelse(is.null(input$x_axis_input) || input$x_axis_input == "",
                          yes = names(revals$color_by_choices[revals$color_by_choices == input$vk_colors]),
                          no = input$x_axis_input)
          
          p <- densityPlot(plot_data(),variable = input$vk_colors, samples = samples, groups = groups,
                           plot_hist = ifelse(input$choose_single == 1, TRUE, FALSE), 
                           xlabel = xlabel, ylabel = input$y_axis_input, title = input$title_input)
        }
        
        #---------- Custom Scatter Plot --------#
        if (input$chooseplots == 'Custom Scatter Plot'){
          validate(need(!is.null(input$whichSamples), message = "Please select at least 1 sample"),
                   need(!is.na(input$vk_colors), message = "Please select a variable to color by"))
          req(!is.null(input$scatter_x), !is.null(input$scatter_y), !("" %in% c(input$scatter_x, input$scatter_y)))
          
          p <- scatterPlot(plot_data(), input$scatter_x, input$scatter_y, colorCName = input$vk_colors, colorPal = colorPal,
                           xlabel = ifelse(is.null(input$x_axis_input) | (input$x_axis_input == ""), 
                                           yes = names(revals$color_by_choices[revals$color_by_choices == input$scatter_x]), 
                                           no = input$x_axis_input), 
                           ylabel = ifelse(is.null(input$y_axis_input) | (input$y_axis_input == ""), 
                                           yes = names(revals$color_by_choices[revals$color_by_choices == input$scatter_y]), 
                                           no = input$y_axis_input),
                           title = input$title_input, legendTitle = revals$legendTitle)
          
        }
        #----------- PCOA Plot ----------#
        if(input$chooseplots==('PCOA Plot')){
          # maximum of 5 pcs or the number of samples - 2 (#columns - ID column - 1)
          xlabel = ifelse(is.null(input$x_axis_input) | (input$x_axis_input == ""), 
                          yes = paste0('PC ', input$scatter_x), 
                          no = input$x_axis_input)
          ylabel = ifelse(is.null(input$y_axis_input) | (input$y_axis_input == ""), 
                          yes = paste0('PC ', input$scatter_y), 
                          no = input$y_axis_input)
          
          pcs <- getPrincipalCoordinates(plot_data(), n_dims = min(5, ncol(plot_data()$e_data)-2), dist_metric = input$choose_dist)
          p <- plotPrincipalCoordinates(pcs, title = input$title_input, x=as.numeric(input$scatter_x), y=as.numeric(input$scatter_y), 
                                        xlabel = xlabel, ylabel=ylabel,
                                        ftmsObj = plot_data(), size = 10)
        }
      }
    })
    # Axes Options
    f <- list(family = "Arial", size = 18, color = "#7f7f7f")
    
    x <- y <- list(titlefont = f)
    
    p <- p %>% layout(xaxis = x, yaxis = y, titlefont = f)
    
    # Null assignment bypasses plotly bug
    p$elementId <- NULL
    
    # ___test-export___
    exportTestValues(plot = p, plot_attrs = p$x$attrs[[p$x$cur_data]], plot_layout = p$x$layout, plot_visdat = p$x$visdat[[p$x$cur_data]]())
    
    # inspect <<- p
    
    revals$current_plot <- p
    
    # use webGL for large plots
    if(peakData2_dim() > max_cells & !(input$chooseplots == 'Density Plot')){
      p <- p %>% layout(margin = list(b = 50, l = 75)) # I dont know why but webGL crops axes titles, must reset
      p <- toWebGL(p)
    }
    
    return(p)
  }),
  # END FXNPLOT
  
  # Axis and title label input menus
  output$title_out <- renderUI({
    
    validate(
      need(!is.null(input$chooseplots), message = "")
    )
    textInput(inputId = "title_input", label = "Plot title", value = "")
  }),
  
  output$x_axis_out <- renderUI({
    validate(
      need(!is.null(input$chooseplots), message = "")
    )
    textInput(inputId = "x_axis_input", label = "X axis label", value = plot_defaults()$xlabel)
  }),
  
  output$y_axis_out <- renderUI({
    validate(
      need(!is.null(input$chooseplots), message = "")
    )
    textInput(inputId = "y_axis_input", label = "Y axis label", value = plot_defaults()$ylabel)
  }),
  
  # legend input
  output$legend_title_out <- renderUI({
    validate(
      need(!is.null(input$chooseplots), message = "")
    )
    if (input$chooseplots == "Density Plot"){
      addCssClass("js_legend_title_input", "grey_out")
      disabled(textInput(inputId = "legend_title_input", label = "Legend label", value = ""))
    }
    else {
      removeCssClass("js_legend_title_input", "grey_out")
      textInput(inputId = "legend_title_input", label = "Legend label", value = "")
    }
    
  }),
  
  # view plot table button UI
  output$view_plots <- renderUI({
    n_plots <- nrow(parmTable$parms[which(rowSums(!is.na(parmTable$parms))!=0),])
    actionButton(inputId = "view_plots_btn", width = '100%', 
                 label = sprintf("View Table Info of %i Saved Plots", n_plots), icon = icon("folder-open", lib = "glyphicon"))
  }),
  
  # color palette selection (main panel)
  output$colorpal_out <- renderUI({
    req(input$choose_single)
    if(!(input$choose_single %in% c(3,4)) | isTRUE(input$chooseplots == "Density Plot")){
      choices = c("YlOrRd", "YlGnBu", "YlGn", "RdYlGn")
      
      extensions <- lapply(choices, function(choice){
        tags$img(src = paste0(choice, ".png"), width = "100px", height = "25px")
      })
    }
    else if(input$choose_single %in% c(3,4)){
      choices = c('default', 'bpr', 'neutral', 'bpg', 'rblkgn')
      fnames = c('default.png', 'bl_prp_rd.png', 'neutral.png', 'bl_pnk_gn.png',	'rd_blk_gn.png')
      extensions <- lapply(fnames, function(fname){
        tags$img(src = fname, width = "100px", height = "25px")
      })
    }
    
    if (isTRUE(input$chooseplots == "Density Plot")){
      addClass("js_colorpal", "grey_out")
      disabled(colored_radiobuttons(inputId = "colorpal", label = "Pick a coloring scheme", inline = TRUE,
                                    choices = choices, extensions = extensions))
    }else {
      removeClass("js_colorpal", "grey_out")
      colored_radiobuttons(inputId = "colorpal", label = "Pick a coloring scheme", inline = TRUE,
                           choices = choices, extensions = extensions)
    }
  })
)