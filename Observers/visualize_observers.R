
### REACTIVE PLOT OPTIONS BELOW MAIN PLOT WINDOW ###

# When plot_data() is recalculated, repopulate the dropdowns under the plot.  Specifically vk_colors and custom scatterplot options.
observeEvent(plot_data(), {

  # store test value
  if (isTRUE(getOption("shiny.testmode"))) {
    revals$plot_data_export <- plot_data()
  }

  ## ifelse block determines how to populate vk_colors dropdown

  # Density plots care not for choose_single!!!!
  if (input$chooseplots == "Density Plot") {
    numeric_cols <- which(sapply(plot_data()$e_meta %>%
      dplyr::select(one_of(emeta_display_choices())), is.numeric))
    color_by_choices <- emeta_display_choices()[numeric_cols]
  }
  else if (input$choose_single == 1) {
    color_by_choices <- emeta_display_choices()

    if (input$chooseplots == "Van Krevelen Plot") {
      color_by_choices <- switch(input$vkbounds,
        'bs1' = c('Van Krevelen Boundary Set' = 'bs1', color_by_choices),
        'bs2' = c('Van Krevelen Boundary Set' = 'bs2', color_by_choices),
        "0" = c('Van Krevelen Boundary Set 1' = 'bs1', 'Van Krevelen Boundary Set 2' = 'bs2', color_by_choices))
    }
  }
  else if (input$choose_single == 2) {

    # create vector of color choices by combining unique elements from e_data and e_meta
    edata_colors <- plot_data()$e_data %>% dplyr::select(-one_of(getEDataColName(plot_data()))) %>% colnames()
    color_by_choices <- c(edata_colors[!(edata_colors %in% emeta_display_choices())], emeta_display_choices())

  } else if (input$choose_single %in% c(3, 4)) {
    color_by_choices <- c("Group membership" = input$summary_fxn)
  }

  # Give default names to unnamed choices
  names(color_by_choices) <- sapply(1:length(color_by_choices), function(i) {
    ifelse(names(color_by_choices[i]) == "" | is.null(names(color_by_choices[i])),
      yes = color_by_choices[i],
      no = names(color_by_choices[i]))
  })

  # if statements which prevent plot from resetting colors/axes when plot is redrawn.
  selected = color_by_choices[1]

  if (input$vk_colors %in% color_by_choices) {
    selected <- input$vk_colors
  }

  selected_x = color_by_choices[color_by_choices != selected][1]
  selected_y = color_by_choices[!(color_by_choices %in% c(selected, selected_x))][1]

  if ((input$scatter_x %in% color_by_choices) & (input$scatter_y %in% color_by_choices)) {
    selected_x <- input$scatter_x
    selected_y <- input$scatter_y
  }

  # Density Colors
  if (input$chooseplots == 'Density Plot') {
    updateSelectInput(session, 'vk_colors', 'Plot Distribution of Variable:',
      choices = color_by_choices,
      selected = selected)
  }

  # Kendrick Colors
  if (input$chooseplots == 'Kendrick Plot') {
    updateSelectInput(session, 'vk_colors', 'Color by:',
      choices = color_by_choices,
      selected = selected)
  }

  # Van Krevelen Colors
  if (input$chooseplots == 'Van Krevelen Plot') {
    updateSelectInput(session, 'vk_colors', 'Color by:',
      choices = color_by_choices,
      selected = selected)
  }

  if (input$chooseplots %in% c('Custom Scatter Plot', 'PCOA Plot')) {
    # allow only numeric columns for the axes but keep categorical coloring options
    numeric_cols <- which(sapply(full_join(plot_data()$e_meta, plot_data()$e_data) %>% dplyr::select(color_by_choices), is.numeric))

    # maintain exclusivity of colorby and x-y variables only in scatterplot
    if (input$chooseplots == 'Custom Scatter Plot') {
      axes_choices <- revals$axes_choices <- color_by_choices[numeric_cols]

      updateSelectInput(session, 'scatter_x', "Horizontal axis variable:",
        choices = axes_choices[!(axes_choices %in% c(input$scatter_y, input$vk_colors))],
        selected = selected_x)
      updateSelectInput(session, "scatter_y", "Vertical axis variable:",
        choices = axes_choices[!(axes_choices %in% c(input$scatter_x, input$vk_colors))],
        selected = selected_y)
      updateSelectInput(session, 'vk_colors', 'Color  by:',
        choices = color_by_choices[!(color_by_choices %in% c(input$scatter_x, input$scatter_y))],
        selected = selected)
    }
    else if (input$chooseplots == 'PCOA Plot') {
      axes_choices <- 1:min(5, ncol(revals$peakData2$e_data) - 2)
      names(axes_choices) <- paste0('PC', axes_choices)
      selected_x <- 1
      selected_y <- 2
    }
  }

  revals$color_by_choices <- color_by_choices

  # The dropdown value will not be updated if this if statement's condition is true, force re-execution of plotting in this case with a reactive var
  if (input$vk_colors %in% color_by_choices) {
    revals$makeplot <- -revals$makeplot
  }

}, priority = 9)
#

##############################################
##### shinyjs helpers and dynamic input updaters
##############################################

# plot selection update on page change
observeEvent(input$top_page, {
  req(input$top_page == "Visualize")
  updateRadioGroupButtons(session, inputId = "chooseplots", selected = revals$chooseplots)
  revals$chooseplots <- input$chooseplots
}, priority = 10)

# shinyjs helpers and reactive value storage for selection inputs
observeEvent(c(input$top_page, input$chooseplots, input$choose_single, input$whichSamples,
  g1_samples(), g2_samples()), {
  req(input$top_page == "Visualize")
  # show/hide dropdowns for sample selection depending on single sample/single group/group comparison
  toggle("js_toggle_groups", condition = input$choose_single %in% c(3, 4))
  toggle("js_toggle_single", condition = input$choose_single %in% c(1, 2))

  # conditionally apply blue outlines to help user along sample selection process
  toggleCssClass("plot_type", "suggest", is.null(input$chooseplots))
  toggleCssClass("plotUI", "suggest", !is.null(input$chooseplots) & input$choose_single == 0)
  toggleCssClass("js_whichSamples", "suggest", any(input$choose_single %in% c(1, 2) & is.null(input$whichSamples),
    input$choose_single == 2 & any(length(input$whichSamples) < 2, is.null(input$whichSamples))))
  toggleCssClass("js_whichGroups1", "suggest", input$choose_single == 3 & is.null(g1_samples()))
  toggleCssClass("js_whichGroups2", "suggest", input$choose_single == 3 & is.null(g2_samples()))

  toggleElement("warnings_visualize", condition = isTRUE(input$choose_single != 0 & !is.null(input$chooseplots)))

  revals$chooseplots <- input$chooseplots
})

# helpers for summary functions
observeEvent(c(input$top_page, input$choose_single, g1_samples(), g2_samples(),
  input$summary_fxn, input$pres_thresh, input$pres_fn, input$absn_thresh, input$pval), {
  req(input$top_page == "Visualize")

  toggleCssClass("js_summary_fxn", "suggest", input$choose_single %in% c(3, 4) & all(!is.null(g1_samples()), !is.null(g2_samples())) & !(input$summary_fxn %in% ftmsRanalysis:::getGroupComparisonSummaryFunctionNames()))

  # conditions different between counts and proportion
  if (isTRUE(input$pres_fn == "nsamps") & isTRUE(input$choose_single %in% c(3, 4))) {
    # g test warning conditions different from pres/absn conditions
    if (isTRUE(input$summary_fxn == "uniqueness_gtest")) {
      # logical conditions that are TRUE if the user did something wrong
      cond_pval <- any(input$pval <= 0, input$pval >= 1)
      cond_pres <- any(input$pres_thresh > min(length(g1_samples()), length(g2_samples())),
        input$pres_thresh < 1,
        !is.numeric(input$pres_thresh))
      toggleCssClass("js_pval", "attention", cond_pval)
      toggleCssClass("js_pres_thresh", "attention", cond_pres)

      # warning message content displayed below dropdowns
      content_pval <- if (isTRUE(cond_pval)) "<p style = 'color:red'>P-value must be between 0 and 1</p>" else NULL
      content_pres <- if (isTRUE(cond_pres)) "<p style = 'color:red'>Presence threshold must be a numeric value of at least 1 and no more than the minimum number of samples in a group</p>" else NULL
      content_absn <- NULL
    }
    else if (isTRUE(input$summary_fxn == "uniqueness_nsamps") & isTRUE(input$choose_single %in% c(3, 4))) {
      cond_pres <- any(input$pres_thresh > min(length(g1_samples()), length(g2_samples())), input$pres_thresh < 1,
        !is.numeric(input$pres_thresh), input$absn_thresh >= input$pres_thresh)
      cond_absn <- any(input$absn_thresh > min(length(g1_samples()), length(g2_samples())) - 1, input$absn_thresh < 0,
        !is.numeric(input$absn_thresh), input$absn_thresh >= input$pres_thresh)

      toggleCssClass("js_pres_thresh", "attention", cond_pres)
      toggleCssClass("js_absn_thresh", "attention", cond_absn)

      content_pval <- NULL
      content_pres <- if (isTRUE(cond_pres)) "<p style = 'color:red'>Presence threshold must be a numeric value of at least 1, no more than the minimum number of samples in a group, and greater than the absence threshold.</p>" else NULL
      content_absn <- if (isTRUE(cond_absn)) "<p style = 'color:red'>Absence threshold must be a numeric value less than the minimum group size and less than the presence threshold.</p>" else NULL
    }
    else content_pval <- content_pres <- content_absn <- NULL
  }
  else if (isTRUE(input$pres_fn == "prop") & isTRUE(input$choose_single %in% c(3, 4))) {
    if (isTRUE(input$summary_fxn == "uniqueness_gtest")) {
      cond_pval <- any(input$pval <= 0, input$pval >= 1)
      cond_pres <- any(input$pres_thresh > 1, input$pres_thresh <= 0, !is.numeric(input$pres_thresh))
      toggleCssClass("js_pval", "attention", cond_pval)
      toggleCssClass("js_pres_thresh", "attention", cond_pres)

      content_pval <- if (isTRUE(cond_pval)) "<p style = 'color:red'>P-value must be between 0 and 1</p>" else NULL
      content_pres <- if (isTRUE(cond_pres)) "<p style = 'color:red'>Presence threshold must be a numeric value greater than 0 and at most 1</p>" else NULL
      content_absn <- NULL
    }
    else if (isTRUE(input$summary_fxn == "uniqueness_prop") & isTRUE(input$choose_single %in% c(3, 4))) {
      cond_pres <- any(input$pres_thresh > 1, input$pres_thresh <= 0, !is.numeric(input$pres_thresh), input$absn_thresh >= input$pres_thresh)
      cond_absn <- any(input$absn_thresh >= 1, input$absn_thresh < 0, !is.numeric(input$absn_thresh), input$absn_thresh >= input$pres_thresh)

      toggleCssClass("js_pres_thresh", "attention", cond_pres)
      toggleCssClass("js_absn_thresh", "attention", cond_absn)

      content_pval <- NULL
      content_pres <- if (isTRUE(cond_pres)) "<p style = 'color:red'>Presence threshold must be a numeric value greater than 0, at most 1, and greater than the absence threshold.</p>" else NULL
      content_absn <- if (isTRUE(cond_absn)) "<p style = 'color:red'>Absence threshold must be a non-negative numeric value less than the presence threshold.</p>" else NULL

    }
    else content_pval <- content_pres <- content_absn <- NULL
  }
  else content_pval <- content_pres <- content_absn <- NULL

  # store warning messages in reactive list.  this list is split and rendered in output$warningmessage_visualize
  revals$warningmessage_visualize$content_pval <- content_pval
  revals$warningmessage_visualize$content_pres <- content_pres
  revals$warningmessage_visualize$content_absn <- content_absn
})

# logical reactive value that clears the plot if a new type is selected
v <- reactiveValues(clearPlot = TRUE)
observeEvent(c(input$chooseplots, input$choose_single, input$whichSamples, g1_samples(), g2_samples()), {
  v$clearPlot <- TRUE
}, priority = 10)
observeEvent(input$plot_submit, {
  v$clearPlot <- FALSE
}, priority = 10)

# color scale inversion
observeEvent(input$flip_colors, {
  toggleCssClass("js_colorpal", "img-hor")
})

# make the options mutually exclusive when doing a comparison of two groups
observeEvent(input$whichGroups2, {
  req(!is.null(revals$peakData2))
  updatePickerInput(session, "whichGroups1", choices = setdiff(names(revals$groups_list), input$whichGroups2), selected = input$whichGroups1)
})
observeEvent(input$whichGroups1, {
  req(!is.null(revals$peakData2))
  updatePickerInput(session, "whichGroups2", choices = setdiff(names(revals$groups_list), input$whichGroups1), selected = input$whichGroups2)
})

# make the options mutually exclusive when doing a comparison of two samples
observeEvent(input$whichSample2, {
  req(!is.null(revals$peakData2))
  updatePickerInput(session, "whichSample1",
    choices = setdiff(colnames(revals$peakData2$e_data[-which(colnames(revals$peakData2$e_data) == getEDataColName(revals$peakData2))]), input$whichSample2),
    selected = input$whichSample1)
})
observeEvent(input$whichSample1, {
  req(!is.null(revals$peakData2))
  updatePickerInput(session, "whichSample2",
    choices = setdiff(colnames(revals$peakData2$e_data[-which(colnames(revals$peakData2$e_data) == getEDataColName(revals$peakData2))]), input$whichSample1),
    selected = input$whichSample2)
})

# Multi purpose observer on input$chooseplots
observeEvent(input$chooseplots, {
  # Pre-populate dropdowns so users can select colors and custom scatterplot axes before submitting plot.
  # Need a vector of the numeric columns to pass to scatterplot
  numeric_cols <- which(sapply(revals$peakData2$e_meta %>% dplyr::select(emeta_display_choices()), is.numeric))

  color_select_label <- if (input$chooseplots == 'Density Plot') "Plot Distribution of Variable:" else "Color by:"

  updateSelectInput(session, 'vk_colors', label = color_select_label, choices = emeta_display_choices(), selected = emeta_display_choices()[1])

  if (input$chooseplots == 'Custom Scatter Plot') {
    updateSelectInput(session, 'scatter_x', choices = emeta_display_choices()[numeric_cols][-3], selected = emeta_display_choices()[numeric_cols][2])
    updateSelectInput(session, 'scatter_y', choices = emeta_display_choices()[numeric_cols][-2], selected = emeta_display_choices()[numeric_cols][3])
  }
  else if (input$chooseplots == 'PCOA Plot') {
    axes_choices <- 1:min(5, ncol(revals$peakData2$e_data) - 2)
    names(axes_choices) <- paste0('PC', axes_choices)
    updateSelectInput(session, 'scatter_x', choices = axes_choices, selected = 1)
    updateSelectInput(session, 'scatter_y', choices = axes_choices, selected = 2)
  }

  # Rest of this observer controls shinyjs disable/enable behavior for reactive plot dropdowns
  dropdown_ids <- c('vkbounds', 'vk_colors', 'scatter_x', 'scatter_y', 'colorpal', 'legend_title_input')
  choices = list('Van Krevelen Plot' = c('vk_colors', 'vkbounds', 'colorpal', 'legend_title_input'),
    'Kendrick Plot' = c('vk_colors', 'colorpal', 'legend_title_input'),
    'Density Plot' = 'vk_colors',
    'Custom Scatter Plot' = c('vk_colors', 'scatter_x', 'scatter_y', 'colorpal', 'legend_title_input'),
    'PCOA Plot' = c('scatter_x', 'scatter_y', 'legend_title_input'),
    'Select an Option' = '0')

  # Toggle axes and coloring options depending on plot type
  lapply(dropdown_ids, function(inputid) {
    toggleState(inputid, condition = inputid %in% choices[[input$chooseplots]])
    toggleCssClass(paste0("js_", inputid), "grey_out", condition = !(inputid %in% choices[[input$chooseplots]]))
  })
})

# maintain mutual exclusivity of scatterplot axes and colors
observeEvent(c(input$scatter_x, input$vk_colors), {
  req(input$chooseplots == 'Custom Scatter Plot')
  updateSelectInput(session, 'scatter_y',
    choices = revals$axes_choices[!(revals$axes_choices %in% c(input$scatter_x, input$vk_colors))],
    selected = input$scatter_y)
})

observeEvent(c(input$scatter_y, input$vk_colors), {
  req(input$chooseplots == 'Custom Scatter Plot')
  updateSelectInput(session, 'scatter_x',
    choices = revals$axes_choices[!(revals$axes_choices %in% c(input$scatter_y, input$vk_colors))],
    selected = input$scatter_x)
})

observeEvent(c(input$scatter_x, input$scatter_y), {
  req(input$chooseplots == 'Custom Scatter Plot')
  updateSelectInput(session, 'vk_colors',
    choices = revals$color_by_choices[!(revals$color_by_choices %in% c(input$scatter_y, input$scatter_x))],
    selected = input$vk_colors)
})

# disable plot_submit and add warning if groups are overlapping
observeEvent(c(input$whichGroups1, input$whichGroups2, input$chooseplots, g1_samples(), g2_samples()), {
  overlap <- intersect(g1_samples(), g2_samples())
  cond_overlap <- length(overlap) != 0

  toggleState('plot_submit', !(cond_overlap & isTRUE(input$choose_single == 3)))
  revals$warningmessage_visualize$group_overlap <- if (cond_overlap & isTRUE(input$choose_single == 3)) sprintf("<p style = color:red>Please choose mutually exclusive groups.  The following samples were present in both groups: %s.</p>", paste(overlap, collapse = ", ")) else NULL

})

# Observer which greys-out colorscale selection if we have not selected a numeric column to color by
observeEvent(numeric_selected(), {
  req(input$chooseplots != "Density Plot")
  if (numeric_selected()) {
    enable("colorpal")
    removeCssClass("js_colorpal", "grey_out")

    enable("legend_title_input")
    removeCssClass("js_legend_title_input", "grey_out")
  }
  else if (!numeric_selected()) {
    if (!(input$choose_single %in% c(3, 4))) {
      disable("colorpal")
      addCssClass("js_colorpal", "grey_out")
    }
    disable("legend_title_input")
    addCssClass("js_legend_title_input", "grey_out")
  }
})

# show extra options panel if we are doing a comparison plot
observeEvent(c(input$choose_single, input$chooseplots), {
  cond <- input$choose_single %in% c(3, 4) & input$chooseplots != 0
  toggle('js_summary_fxn', condition = cond)
})

### Summary comparison plot selection control ###
observeEvent(c(input$pres_fn, g1_samples(), g2_samples(), input$choose_single), {

  cond_smallgrp <- any(length(g1_samples()) < 3, length(g2_samples()) < 3) & isTRUE(input$choose_single %in% c(3, 4)) & input$chooseplots != "Density Plot"
  # cond_onesample <- any(length(input$whichGroups1) < 2, length(input$whichGroups2) < 2) & isTRUE(input$choose_single == 3) & input$chooseplots != "Density Plot"
  content <- if (cond_smallgrp & isTRUE(input$summary_fxn == "uniqueness_gtest")) "<p style = 'color:deepskyblue'>G-test disabled for groups with less than 3 samples</p>" else NULL
  # content_onesample <- if(cond_onesample) "style = 'color:deepskyblue'>Input at least 2 samples per group for group comparison." else NULL

  if (isTRUE(input$pres_fn == "nsamps")) {
    if (cond_smallgrp) {
      choices = c("Select one" = "select_none", "Presence/absence thresholds" = "uniqueness_nsamps")
    }
    else choices = c("Select one" = "select_none", "G test" = "uniqueness_gtest", "Presence/absence thresholds" = "uniqueness_nsamps")
    updateNumericInput(session, "thresh", min = 1, max = min(length(g1_samples()), length(g2_samples())))
  }
  else if (isTRUE(input$pres_fn == "prop")) {
    if (cond_smallgrp) {
      choices = c("Select one" = "select_none", "Presence/absence thresholds" = "uniqueness_prop")
    }
    else choices = c("Select one" = "select_none", "G test" = "uniqueness_gtest", "Presence/absence thresholds" = "uniqueness_prop")
    updateNumericInput(session, "thresh", min = 0, max = 1)
  }
  else choices = NULL

  selected = if (isTRUE(input$summary_fxn %in% c("uniqueness_nsamps", "uniqueness_prop"))) choices["Presence/absence thresholds"]
  else if (!cond_smallgrp & isTRUE(input$summary_fxn %in% c("uniqueness_gtest"))) choices["G test"]
  else choices["Select one"]

  updateSelectInput(session, "summary_fxn", choices = choices, selected = selected)
  revals$warningmessage_visualize$small_groups <- content
  # revals$warningmessage_visualize$one_sample <- content_onesample
})

# Control state for presence/absence threshold and p-value inputs
observeEvent(input$summary_fxn, {
  req(input$chooseplots != "Density Plot")
  toggleState("pval", input$summary_fxn == "uniqueness_gtest")
  toggleCssClass("js_pval", "grey_out", condition = input$summary_fxn != "uniqueness_gtest")
  toggleState("absn_thresh", input$summary_fxn != "uniqueness_gtest")
  toggleCssClass("js_absn_thresh", "grey_out", condition = input$summary_fxn == "uniqueness_gtest")
})

# Control coloring choices depending on vk bounds selection
# NOTE:  fxnplot does NOT redraw when input$vkbounds is invalidated.  The dependency is entirely through revals$makeplot
observeEvent(input$vkbounds, {
  req(isTRUE(input$chooseplots == "Van Krevelen Plot"))
  if (isTRUE(input$choose_single == 1)) {
    if (input$vkbounds == 0) {
      selected <- input$vk_colors
      updateSelectInput(session, "vk_colors",
        choices = c('Van Krevelen Boundary Set 1' = 'bs1', 'Van Krevelen Boundary Set 2' = 'bs2', revals$color_by_choices[!(revals$color_by_choices %in% c("bs1", "bs2"))]),
        selected = input$vk_colors)
    }
    else if (input$vkbounds == 'bs1') {
      selected <- if (input$vk_colors == "bs2") NULL else input$vk_colors
      updateSelectInput(session, "vk_colors",
        choices = c('Van Krevelen Boundary Set' = 'bs1', revals$color_by_choices[!(revals$color_by_choices %in% c("bs1", "bs2"))]),
        selected = selected)
    }
    else if (input$vkbounds == "bs2") {
      selected <- if (input$vk_colors == "bs1") NULL else input$vk_colors
      updateSelectInput(session, "vk_colors",
        choices = c('Van Krevelen Boundary Set' = 'bs2', revals$color_by_choices[!(revals$color_by_choices %in% c("bs1", "bs2"))]),
        selected = selected)
    }

    # still want to redraw if the colors didn't change, fxnplot does NOT invalidate on input$vkbounds
    if (isTRUE(selected == input$vk_colors)) {
      revals$makeplot <- -revals$makeplot
    }
  }
  else if (isTRUE(input$choose_single > 1)) {
    revals$makeplot <- -revals$makeplot
  }
})

# Observer which stores sample selections so user (me testing the app) doesn't have to re-input
observeEvent(c(input$whichGroups1, input$whichGroups2, input$whichSamples, input$whichSample1, input$whichSample2), {
  revals$group_1 <- input$whichGroups1
  revals$group_2 <- input$whichGroups2
  if (isTRUE(input$choose_single == 1)) revals$single_sample <- input$whichSamples
  if (isTRUE(input$choose_single == 2)) revals$single_group <- input$whichSamples
  if (isTRUE(input$choose_single == 4)) {
    revals$sample_1 <- input$whichSample1
    revals$sample_2 <- input$whichSample2
  }
})
####                                 ###
