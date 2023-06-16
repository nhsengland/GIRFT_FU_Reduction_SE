
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

month_commencing <- calendar %>% 
  filter(day_num == 1) %>% 
  select(date,
         year_num,
         month_name_short,
         fin_year,
         fin_month)

combined <- left_join(x = actuals_3_years,
                      y = month_commencing,
                      by = join_by(month_commencing == date),
                      keep = FALSE)

combined <- combined %>% 
  select(month_commencing,
         year_num,
         fin_year,
         month_name_short,
         fin_month,
         der_provider_code,
         stp_code,
         activity) %>% 
  mutate(der_provider_code = substr(der_provider_code,1,3)) %>% 
  group_by(month_commencing,
           year_num,
           fin_year,
           month_name_short,
           fin_month,
           der_provider_code,
           stp_code) %>% 
  summarise(activity = sum(activity)) %>% 
  ungroup()