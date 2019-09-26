list(
  ## Sidebar Panel ##
  # Plot options, with selections removed if the necessary columns in e_meta are not present.
  output$plot_type <- renderUI({
    input$top_page
    validate(need(revals$peakData2, "A peakData object was not found, please check that you have successfully uploaded data"))
    req(input$top_page == "Visualize")
    
    choices <- c('Van Krevelen Plot', 'Kendrick Plot', 'Density Plot', 'Custom Scatter Plot', 'PCOA Plot')
    
    # disallow kendrick plots if either kmass or kdefect not calculated/present in emeta
    if (is.null(attr(revals$peakData2, "cnames")$kmass_cname) | is.null(attr(revals$peakData2, "cnames")$kdefect_cname)){
      choices <- choices[choices != "Kendrick Plot"]
    }
    
    # disallow vk plots if o:c or h:c ratios not calculated/present in emeta or only contain zeros/NA's
    if (is.null(attr(revals$peakData2, "cnames")$o2c_cname) | is.null(attr(revals$peakData2, "cnames")$h2c_cname)){
      choices <- choices[choices != "Van Krevelen Plot"]
    }
    else if(any(all(revals$peakData2$e_meta[[attr(revals$peakData2, "cnames")$o2c_cname]] %in% c(0,NA)),
                all(revals$peakData2$e_meta[[attr(revals$peakData2, "cnames")$h2c_cname]] %in% c(0,NA)))){
      choices <- choices[choices != "Van Krevelen Plot"]
    }
    
    # disallow density plots if there are no numeric columns
    if (!any(sapply(revals$peakData2$e_meta %>% dplyr::select(-one_of(getEDataColName(revals$peakData2))), is.numeric))){
      choices <- choices[choices != c("Density Plot", "Custom Scatter Plot")]
    }
    
    # disallow custom scatter plot if we have large data
    if (peakData2_dim() > max_cells){
      choices <- choices[choices != 'Custom Scatter Plot']
    }
    
    # disallow pcoa plot for 1 sample data
    if (nrow(revals$peakData2$f_data) < 2){
      choices <- choices[choices != 'PCOA Plot']
    }
    
    #if everything is disallowed, give warning and silently stop execution.
    if (length(choices) == 0) return(tags$p("There is not enough information in the molecular identification file to produce any plots.  Choose more variables to calculate in the preprocess tab or append some metadata to the molecular identification file prior to uploading", style = "color:gray"))
    
    selectInput('chooseplots', 'Choose a plot type',
                choices = choices, 
                selected = 0
    )
  }),
  
  # Logic to force single sample selection in the case where only 1 sample is present
  output$plotUI <- renderUI({
    req(!is.null(input$chooseplots))
    if (nrow(revals$peakData2$f_data) == 1 | (input$chooseplots %in% c('Custom Scatter Plot', 'PCOA Plot'))){
      return(tagList(
        tags$div(class = 'grey_out',
                 hidden(selectInput('choose_single', 'I want to plot using:',
                                    choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples by group' = 2, 
                                                'A comparison of groups' = 3, 'A comparison of two samples' = 4),
                                    selected = 1))
        )
        #tags$p("No grouping options for custom scatter plots and single sample datasets.", style = "color:gray;font-size:small;margin-top:3px")
      ))
    }
    else {
      return(selectInput('choose_single', 'I want to plot using:',
                         choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples' = 2,
                                     'A comparison of groups' = 3, 'A comparison of two samples' = 4),
                         selected = 0))
    }
  }),
  
  # pcoa distance metric dropdown
  output$pcoa_dist <- renderUI({
    req(input$chooseplots=='PCOA Plot')
    dist_choices = c('manhattan', 'euclidean', 'canberra', 'clark', 'bray', 'kulczynski', 
                     'jaccard', 'gower', 'altGower', 'morisita', 'horn', 'mountford', 'raup', 
                     'binomial', 'cao','mahalanobis')
    
    selectInput('choose_dist', "Choose a distance metric", choices = dist_choices, selected = 'bray')
  }),
  
  # select groups to color by
  output$viztab_select_groups <- renderUI({
    req(input$chooseplots=='PCOA Plot')
    pickerInput("viztab_select_groups", "Select groups:", 
                choices = names(revals$groups_list),
                multiple = TRUE
    )
  }),
  
  ## UI outputs for group/sample comparisons ##
  ## conditional display depending on whether comparing two samples or two groups ## 
  output$plotUI_comparison_1 <- renderUI({
    req(input$choose_single != 0, !is.null(input$chooseplots))
    if(input$choose_single == 3){
      choice_diff <- setdiff(names(revals$groups_list), isolate(input$whichGroups2))
      name_label = HTML("<input type = 'text' id = 'group1_name' placeholder = 'Group 1' style = 'border-style:solid;border-width:1px;'/>")
      pickerInput('whichGroups1', name_label,
                  choices = choice_diff,
                  selected = if(is.null(isolate(revals$group_1))) choice_diff[1] else isolate(revals$group_1),
                  options =  pickerOptions(dropupAuto = FALSE, actionsBox = TRUE))
    }
    else if(input$choose_single == 4){
      choice_diff <- setdiff(colnames(revals$peakData2$e_data[-which(colnames(revals$peakData2$e_data) == getEDataColName(revals$peakData2))]), isolate(input$whichSample2))
      pickerInput('whichSample1', "Sample 1:",
                  choices = choice_diff,
                  selected = if(is.null(isolate(revals$whichSample1))) choice_diff[1] else isolate(revals$whichSample1),
                  options =  pickerOptions(dropupAuto = FALSE))
    }
  }),
  
  output$plotUI_comparison_2 <- renderUI({
    req(input$choose_single != 0, !is.null(input$chooseplots))
    if(input$choose_single == 3){
      choice_diff <- setdiff(names(revals$groups_list), isolate(input$whichGroups1))
      name_label = HTML("<input type = 'text' id = 'group2_name' placeholder = 'Group 2' style = 'border-style:solid;border-width:1px;'/>")
      pickerInput("whichGroups2", name_label, 
                  choices = setdiff(names(revals$groups_list), isolate(input$whichGroups1)),
                  selected = if(is.null(isolate(revals$group_2))) choice_diff[2] else isolate(revals$group_2),
                  options =  pickerOptions(dropupAuto = FALSE, actionsBox = TRUE))
    }
    else if(input$choose_single == 4){
      choice_diff <- setdiff(colnames(revals$peakData2$e_data[-which(colnames(revals$peakData2$e_data) == getEDataColName(revals$peakData2))]), isolate(input$whichSample1))
      pickerInput("whichSample2", "Sample 2:", 
                  choices = choice_diff,
                  selected = if(is.null(isolate(revals$whichSample2))) choice_diff[2] else isolate(revals$whichSample2),
                  options =  pickerOptions(dropupAuto = FALSE))
    }
  }),
  
  ##
  
  # UI output for single sample or single group
  output$plotUI_single <- renderUI({
    req(input$choose_single != 0, !is.null(input$chooseplots), input$chooseplots != 'PCOA Plot')
    if(input$choose_single == 2){
      tagList(
        div(id = "js_whichSamples",
            pickerInput('whichSamples', 'Grouped Samples',
                        choices = colnames(revals$peakData2$e_data %>% dplyr::select(-one_of(getEDataColName(revals$peakData2)))),
                        multiple = TRUE, selected = isolate(revals$single_group), 
                        options =  pickerOptions(dropupAuto = FALSE, actionsBox = TRUE))),
        conditionalPanel(
          condition = 'input.whichSamples.length < 2',
          tags$p("Please select at least 2 samples", style = "color:gray")
        )# End conditional output multiple samples#
      )
    }
    else return(div(id = "js_whichSamples", selectInput('whichSamples', 'Sample', 
                                                        choices = colnames(revals$peakData2$e_data %>% dplyr::select(-one_of(getEDataColName(revals$peakData2)))), 
                                                        selected = revals$single_sample)))
  }),
  
  # selector for summary funcion
  output$summary_fxn_out <- renderUI({
    req(!(input$choose_single %in% c(1,2)), cancelOutput = T)
    text_pres_fn <- "For a given peak, should the count or proportion of nonmissing values across samples in a group be used to determine whether or not that peak is present/absent within the group"
    text_test <- HTML("<p>Should a G-test or presence absence thresholds be used to determine whether a sample is unique to a particular group?</p><p>Depending on your selection, you will be asked for a presence threshold and a p-value (G-test) or a presence AND absence threshold<p/>") 
    
    # density plot has group summary options disabled
    if (isTRUE(input$chooseplots == "Density Plot")){
      summary_dropdown <- tags$div(class = "grey_out",
                                   tags$p("No summary functions for comparison density plots", style = "color:gray;font-size:small;margin-top:3px;font-weight:bold"),
                                   hidden(
                                     radioButtons("pres_fn", 
                                                  div("Determine presence/absence by:", div(style = "display:inline-block;right:5px", tipify(icon("question-sign", lib = "glyphicon"), title = text_pres_fn, placement = "top", trigger = 'hover'))), 
                                                  choices = c("No. of Samples Present" = "nsamps", "Proportion of Samples Present" = "prop"), inline = TRUE, selected = "nsamps")
                                   ),
                                   
                                   hidden(selectInput("summary_fxn", 
                                                      div("Determine uniqueness using:", div(style = "display:inline-block", tipify(icon("question-sign", lib = "glyphicon"), title = text_test, placement = "top", trigger = 'hover'))), 
                                                      choices = c("Select one" = "select_none", "G test" = "uniqueness_gtest", "Presence/absence thresholds" = "uniqueness_nsamps"), selected = "select_none")),
                                   hidden(numericInput("pres_thresh", "Presence threshold", value = 1, step = 0.1)),
                                   hidden(numericInput("absn_thresh", "Absence threshold", value = 0, step = 0.1)),
                                   hidden(numericInput("pval", "p-value", min = 0, max = 1, value = 0.05, step = 0.1))
      )
      # non-density plots 
    }else{ 
      summary_dropdown <- tagList(
        hr(style = 'margin-top:2px;height:3px;background-color:#1A5276'),
        radioButtons("pres_fn", 
                     div("Determine presence/absence by:", div(style = "color:deepskyblue;display:inline-block", tipify(icon("question-sign", lib = "glyphicon"), title = text_pres_fn, placement = "top", trigger = 'hover'))), 
                     choices = c("No. of Samples Present" = "nsamps", "Proportion of Samples Present" = "prop"), inline = TRUE, selected = "nsamps"),
        
        hr(style = "margin-top:2px"),
        
        div(id = "js_summary_fxn", selectInput("summary_fxn", 
                                               div("Determine uniqueness using:", div(style = "color:deepskyblue;display:inline-block", tipify(icon("question-sign", lib = "glyphicon"), title = text_test, placement = "top", trigger = 'hover'))), 
                                               choices = c("Select one" = "select_none", "G test" = "uniqueness_gtest", "Presence/absence thresholds" = "uniqueness_nsamps"))),
        
        splitLayout(class = "squeezesplitlayout", 
                    div(id = "js_pres_thresh", numericInput("pres_thresh", "Presence threshold", value = 1, step = 0.1)),
                    div(id = "js_absn_thresh", numericInput("absn_thresh", "Absence threshold", value = 0, step = 0.1)),
                    div(id ="js_pval", numericInput("pval", "p-value", min = 0, max = 1, value = 0.05, step = 0.1))
        )
      )}
    return(summary_dropdown)
  })
)