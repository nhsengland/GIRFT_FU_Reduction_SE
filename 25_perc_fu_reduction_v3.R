## Load libraries
source('helper_files\\libraries.R')

## Import data
source('helper_files\\csv_data_import.R')

## set up lookups
source('helper_files\\lookups.R')

## all to lower
source('helper_files\\all_to_lower.R')

## create calendar and working days calendar
source('helper_files\\calendar_builder.R')

## add a set of local colours for tables and rag ratings
source('helper_files\\local_colours.R')

## main script to manipulate data and create data frames
source('helper_files\\create_data_frames.R')

######################

## create a list of systems for the parameters

icb_list <- lkup_system_names %>% 
  filter(!short_name == 'Non_SE') %>% 
  pull(short_name)

## set the region name
region_name  <- 'se_region'
icb_list <- c(icb_list,region_name)
full_region_name <- 'the South East region'

## for the parameters we will pull each system once
## then collapse the list into a set to call all of them for the region view

for(icb in icb_list) {
  paramicb <- icb
  rmarkdown::render('girft_fu_reduction.Rmd', 
                    output_file = paste0("op-fu_reduction_",paramicb, "_", format(Sys.Date(),'%d%m%Y')),
                    output_dir = "Output", 
                    params = list(icb = paramicb))
  }