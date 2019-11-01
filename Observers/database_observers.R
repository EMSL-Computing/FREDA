# create the mapping on button click
observeEvent(input$create_mapping, {
  disable('create_mapping')
  on.exit({
    enable('create_mapping')
  })
  
  req(revals$peakData2, !is.null(attr(revals$peakData2, 'cnames')$mf_cname))
  
  # Thresholds for excluding results with too many returned elements.  Empty threshold = infinity
  maxreacts <- ifelse(is.na(input$maxreacts) | is_empty(input$maxreacts), Inf, input$maxreacts)
  maxmods <- ifelse(is.na(input$maxmods) | is_empty(input$maxmods), Inf, input$maxmods)
  maxpaths <- ifelse(is.na(input$maxpaths) | is_empty(input$maxpaths), Inf, input$maxpaths)
  
  ######## KEGG #########
  tryCatch({
    if(input$database_select == 'Kegg'){
      if(!exists('kegg_compounds')){
        data('kegg_compounds')
      }
      
      # get list of all formulae and subset kegg_compounds to identified formulae
      forms <- revals$peakData2$e_meta %>% 
        filter(!is.na(!!rlang::sym(getMFColName(revals$peakData2)))) %>% 
        dplyr::rename(FORMULA = !!rlang::sym(getMFColName(revals$peakData2))) %>%
        dplyr::select(getEDataColName(revals$peakData2), FORMULA)
      
      # peaks to compounds
      kegg_sub <- forms %>% 
        left_join(kegg_compounds, by = 'FORMULA') %>% 
        filter(!is.na(COMPOUND) | !is.na(REACTION)) %>%
        tibble::as_tibble() %>%
        dplyr::select(getEDataColName(revals$peakData2), COMPOUND, FORMULA, URL) %>%
        mutate(URL = paste0("<a href='",URL,"' target='_blank'>", 'compound link</a>'))
      
      ### Three conditionals, each which add a column where each row element is a list of all related 
      
      if(input$comp2react){
        kegg_sub <- kegg_sub %>% 
          mutate(REACTION = map(COMPOUND, newcol_from_mapping, maxlen = maxreacts, map_list = 'kegg_compound_reaction_map'))
      }
      
      # compounds to modules
      if(input$react2mod){
        req(input$comp2react)
        kegg_sub <- kegg_sub %>% 
          mutate(MODULE = map(REACTION, newcol_from_mapping, maxlen = maxmods, map_list = 'kegg_reaction_module_map'))
      }
      
      # modules to pathways
      if(input$mod2path){
        req(input$react2mod, input$comp2react)
        kegg_sub <- kegg_sub %>% 
          mutate(PATHWAY = map(MODULE, newcol_from_mapping, maxlen = maxpaths, map_list = 'kegg_module_pathway_map'))
      }
      
      ## conditional block which unnests calculated columns based on unique row selection
      # If none, simply create semicolon separated values of the elements in the three list columns
      if(input$which_unique == 'None'){
        if('REACTION' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>% mutate(REACTION = map(REACTION, list2semicolon))
        }
        if('MODULE' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>% mutate(MODULE = map(MODULE, list2semicolon))
        }
        if('PATHWAY' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>% mutate(PATHWAY = map(PATHWAY, list2semicolon))
        }
      }
      # Unnests REACTION and creates ;-collapsed versions of MODULE and PATHWAY
      else if(input$which_unique == 'REACTION'){
        kegg_sub <- kegg_sub %>% 
          tidyr::unnest(REACTION, .drop = F) %>% 
          tidyr::unnest(REACTION, .drop = F)
        
        if('MODULE' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>% mutate(MODULE = map(MODULE, list2semicolon))
        }
        
        if('PATHWAY' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>% mutate(PATHWAY = map(PATHWAY, list2semicolon))
        }
        
        # Display EC numbers if unique reactions
        kegg_sub <- kegg_sub %>% 
          left_join(kegg_reactions %>% dplyr::select(REACTION, ENZYME)) %>% 
          mutate(ENZYME = gsub('[[:space:]]+', ';', ENZYME))
        
      }
      # Unnests REACTION and MODULE and creates ;-collapsed versions of PATHWAY
      else if(input$which_unique == 'MODULE'){
        if('REACTION' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>%
            tidyr::unnest(REACTION, .drop = F) %>% 
            tidyr::unnest(REACTION, .drop = F)
        }
        
        if('MODULE' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>%
            mutate(MODULE = map2(REACTION, MODULE, unnest_by_key)) %>%
            tidyr::unnest(MODULE, .drop = F) %>% 
            tidyr::unnest(MODULE, .drop = F)
        }
        
        if('PATHWAY' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>%
            mutate(PATHWAY = map2(MODULE, PATHWAY, unnest_by_key)) %>% 
            mutate(PATHWAY = map(PATHWAY, list2semicolon))
        }
        
        # display class if modules are selected as unique
        kegg_sub <- kegg_sub %>% 
          left_join(kegg_modules %>% dplyr::select(MODULE, CLASS)) %>% 
          rowwise() %>% 
          mutate(CLASS = strsplit(CLASS, '\n')[[1]] %>% # get first row
          {strsplit(.,';')[[1]]} %>% # split by semicolons
            .[[length(.)]]) # get LAST element of semicolon separated list
      }
      
      # Unnests everything
      else if(input$which_unique == 'PATHWAY'){
        
        if('REACTION' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>%
            tidyr::unnest(REACTION, .drop = F) %>% 
            tidyr::unnest(REACTION, .drop = F)
        }
        
        if('MODULE' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>%
            mutate(MODULE = map2(REACTION, MODULE, unnest_by_key)) %>%
            tidyr::unnest(MODULE, .drop = F) %>% 
            tidyr::unnest(MODULE, .drop = F)
        }
        
        if('PATHWAY' %in% colnames(kegg_sub)){
          kegg_sub <- kegg_sub %>%
            mutate(PATHWAY = map2(MODULE, PATHWAY, unnest_by_key)) %>%
            tidyr::unnest(PATHWAY, .drop = F) %>% 
            tidyr::unnest(PATHWAY, .drop = F)
        }
        
        # display 'NAME' column if pathways are selected
        kegg_sub <- kegg_sub %>% 
          left_join(kegg_pathways %>% dplyr::select(PATHWAY, NAME))
        
      }
      
      # tidy columns
      column_order <- c(getEDataColName(revals$peakData2), 'COMPOUND', 'FORMULA', 'URL', 'REACTION', 'ENZYME', 'MODULE', 'CLASS', 'PATHWAY', 'NAME')
      column_order <- column_order[which(column_order %in% colnames(kegg_sub))]
      
      tables$kegg_table <- kegg_sub %>% dplyr::select(column_order)
      updateRadioGroupButtons(session, 'which_table', selected = 1)
    }
    
    #####################
    ###### METACYC ######
    #####################
    
    else if(input$database_select == 'MetaCyc'){
      if(!exists('mc_compounds')){
        data('mc_compounds')
      }
      
      # map peaks to compounds
      mc_sub <- revals$peakData2 %>% 
        mapPeaksToCompounds() %>%
        {dplyr::select(.$e_meta, getEDataColName(revals$peakData2), getEDataColName(.), getCompoundColName(.))} %>%
        tibble::as_tibble()
      
      # compounds to reactions
      if(input$comp2react){
        mc_sub <- mc_sub %>% 
          mutate(REACTION = map(Compound, newcol_from_mapping, maxlen = maxreacts, 'mc_compound_reaction_map'))
      }
      
      if(input$react2mod){
        req(input$comp2react)
        mc_sub <- mc_sub %>% 
          mutate(MODULE = map(REACTION, newcol_from_mapping, maxlen = maxmods, map_list = 'mc_reaction_module_map'))
      }
      
      if(input$mod2path){
        req(input$comp2react, input$react2mod)
        mc_sub <- mc_sub %>%
          mutate(SUPERPATHWAY = map(MODULE, newcol_from_mapping, maxlen = maxpaths, map_list = 'mc_module_superpathway_map'))
      }
      
      ## unnest block (MetaCyc)
      
      if(input$which_unique == 'None'){
        if('REACTION' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>% mutate(REACTION = map(REACTION, list2semicolon))
        }
        if('MODULE' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>% mutate(MODULE = map(MODULE, list2semicolon))
        }
        if('SUPERPATHWAY' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>% mutate(SUPERPATHWAY = map(SUPERPATHWAY, list2semicolon))
        }
      }
      # Unnests REACTION and creates ;-collapsed versions of MODULE and SUPERPATHWAY
      else if(input$which_unique == 'REACTION'){
        mc_sub <- mc_sub %>% 
          tidyr::unnest(REACTION, .drop = F) %>% 
          tidyr::unnest(REACTION, .drop = F)
        
        if('MODULE' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>% mutate(MODULE = map(MODULE, list2semicolon))
        }
        
        if('SUPERPATHWAY' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>% mutate(SUPERPATHWAY = map(SUPERPATHWAY, list2semicolon))
        }
      }
      # Unnests REACTION and MODULE and creates ;-collapsed versions of SUPERPATHWAY
      else if(input$which_unique == 'MODULE'){
        if('REACTION' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>%
            tidyr::unnest(REACTION, .drop = F) %>% 
            tidyr::unnest(REACTION, .drop = F)
        }
        
        if('MODULE' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>%
            mutate(MODULE = map2(REACTION, MODULE, unnest_by_key)) %>%
            tidyr::unnest(MODULE, .drop = F) %>% 
            tidyr::unnest(MODULE, .drop = F)
        }
        
        if('SUPERPATHWAY' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>%
            mutate(SUPERPATHWAY = map2(MODULE, SUPERPATHWAY, unnest_by_key)) %>% 
            mutate(SUPERPATHWAY = map(SUPERPATHWAY, list2semicolon))
        }
        
      }
      # Unnests everything
      else if(input$which_unique == 'SUPERPATHWAY'){
        
        if('REACTION' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>%
            tidyr::unnest(REACTION, .drop = F) %>% 
            tidyr::unnest(REACTION, .drop = F)
        }
        
        if('MODULE' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>%
            mutate(MODULE = map2(REACTION, MODULE, unnest_by_key)) %>%
            tidyr::unnest(MODULE, .drop = F) %>% 
            tidyr::unnest(MODULE, .drop = F)
        }
        
        if('SUPERPATHWAY' %in% colnames(mc_sub)){
          mc_sub <- mc_sub %>%
            mutate(SUPERPATHWAY = map2(MODULE, SUPERPATHWAY, unnest_by_key)) %>%
            tidyr::unnest(SUPERPATHWAY, .drop = F) %>% 
            tidyr::unnest(SUPERPATHWAY, .drop = F)
        }
      }
      
      tables$mc_table <- mc_sub
      updateRadioGroupButtons(session, 'which_table', selected = 2)
      
    }
  },
  error = function(e){
    msg = paste0('Error creating the kegg mapping: \n System error: ', e)
    revals$warningmessage_database$mapping_error <<- sprintf("<p style = 'color:red'>%s</p>", msg)
  })
  
  updateCollapse(session, 'database_tables_parent_collapse', open = 'database_tables')
  
})

####### MODAL FUNCTIONALITY #######

# show modal with table info
observeEvent(input$view_db_tables, {
  showModal(
    modalDialog(
      tagList(
        tags$h3('Select a table to see a preview:'),
        DTOutput('saved_db_table'), # info on saved tables
        hr(),
        DTOutput('selected_db_table') # display selected table
      ),
      footer = tagList(
        div(style = 'float:left', 
            bsButton('remove_db_table', 'Remove selected table', icon = icon('remove'))
        ),
        modalButton("Dismiss")
      ),
      size = 'l'
    )
  )
})

# save tables to a list, maximum 5 allowed
observeEvent(input$save_db_table, {
  
  # display warning message and retun NULL if they've stored too many tables
  revals$warningmessage_database$too_many_tables <- NULL
  
  if(length(tables$mapping_tables) >= 5){
    msg = paste0('Maximum of 5 tables, please remove some of your saved tables')
    revals$warningmessage_database$too_many_tables <<- sprintf("<p style = 'color:red'>%s</p>", msg)
    return(NULL)
  }
  
  kegg_or_mc <- ifelse(input$which_table == 1, 'Kegg', 'Metacyc')
  table_name = sprintf('%s Table %s', kegg_or_mc, input$save_db_table)
  
  if(input$which_table == 1 & !is.null(tables$kegg_table)){
    tables$mapping_tables[[table_name]] <- tables$kegg_table
    tables$saved_db_info[nrow(tables$saved_db_info) + 1,] <- c(table_name, nrow(tables$kegg_table), paste(colnames(tables$kegg_table), collapse = ';')) 
  }
  else if(input$which_table == 2 & !is.null(tables$mc_table)){
    tables$mapping_tables[[table_name]] <- tables$mc_table
    tables$saved_db_info[nrow(tables$saved_db_info) + 1,] <- c(table_name, nrow(tables$mc_table), paste(colnames(tables$mc_table), collapse = ';'))
  }
  
  addCssClass("view_db_tables", "pulse_bow")
  Sys.sleep(0.6)
  removeCssClass("view_db_tables", "pulse_bow")
  
})

# remove the selected database table on button click
# need to remove the entry tables$saved_db_info and the corresponding table in tables$mapping_tables
observeEvent(input$remove_db_table, {
  req(length(input$saved_db_table_rows_selected) > 0)
  table_name = tables$saved_db_info[input$saved_db_table_rows_selected,'Tables']
  
  tables$saved_db_info <- tables$saved_db_info %>% filter(Tables != table_name)
  tables$mapping_tables[[table_name]] <- NULL
})

################


