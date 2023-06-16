
#load library file
source('helper_files//libraries.R')

#load calendar file
source('helper_files//calendar_builder.R')

#import data
actuals_3_years <- read_csv("data/actuals_3_years.csv", 
                            col_types = cols(Month_commencing = col_date(format = "%d/%m/%Y"), 
                                             Treatment_Function_Code = col_character()))

#import the lookups
source('helper_files//lookups.R')

#set everything to lower case 
source('helper_files//all_to_lower.R')

