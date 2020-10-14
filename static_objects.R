# static variables #
dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'

dbe_opts_info <- 'Semicolon separated strings of elements and their valences, i.e. to calculate the dbe for two sets of valences, input C4H1N3O2S2P3;C3H3N2O2S4P4'
kendrick_opts_info <- 'The base compound(s) used to calculate the Kendrick Mass.  See <a href = ""https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5447562/"">[Foquet and Sato, 2017]</a> for details.'

dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'

ttip_text = list("plot_save"="Save the last created plot", "plot_review"="Review saved plots", "page_help"="How do I use this page?")

#------ Download Example Data ---------#
example_edata <- read_csv('Data/example12T_edata.csv') %>% as.data.frame(stringsAsFactors = FALSE)
example_emeta <- read_csv('Data/example12T_emeta.csv') %>% as.data.frame(stringsAsFactors = FALSE)
calc_opts <- read_csv('calculation_options.csv') %>% as.data.frame(stringsAsFactors = FALSE)
calc_vars <- read_csv('calculation_variables.csv') %>% as.data.frame(stringsAsFactors = FALSE)
# determines when 'large data' options are triggered
max_cells <- 2000000
