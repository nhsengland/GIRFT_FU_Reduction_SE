library(tidyverse)

convert_df_colnames_to_lower <- function() {
  # List all objects in the global environment
  all_objects <- ls()
  
  # Filter out the data frames and spec_tbl_df objects
  data_frames <- as.logical(sapply(all_objects, function(x) {
    is.data.frame(get(x)) | inherits(get(x), "spec_tbl_df")
  }))
  
  # Get the names of the data frames
  df_names <- all_objects[data_frames]
  
  # Convert column names to lowercase
  for (df_name in df_names) {
    df <- get(df_name)
    names(df) <- tolower(names(df))
    assign(df_name, df, envir = globalenv())
  }
}


## Import data
baseline_excl_march <- read_csv("data/baseline_excl_march.csv", 
                                col_types = cols(Month_commencing = col_date(format = "%d/%m/%Y"), 
                                                 Treatment_Function_Code = col_character()))

months_for_march_baseline <- read_csv("data/months_for_march_baseline.csv", 
                                      col_types = cols(Month_commencing = col_date(format = "%d/%m/%Y"), 
                                                       Treatment_Function_Code = col_character()))

actuals_2022_23 <- read_csv("data/actuals_2022_23.csv", 
                            col_types = cols(Month_commencing = col_date(format = "%d/%m/%Y"), 
                                             Treatment_Function_Code = col_character()))

## set up lookups
lkup_provider_names <- read_csv("lookups/provider_short_names.csv")
lkup_system_names <- read_csv("lookups/system_names.csv")
lkup_tfc_list <- read_csv("lookups/tfc_list.csv")

convert_df_colnames_to_lower() # Call the function

## Perform analysis on data frames below