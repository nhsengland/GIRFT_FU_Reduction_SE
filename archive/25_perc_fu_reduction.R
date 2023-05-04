library(kableExtra)
library(tidyverse)

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

## List all objects in the global environment
all_objects <- ls()

## Filter out the non-data frames and non-spec_tbl_df objects
data_frames <- sapply(all_objects, function(x) {
  is.data.frame(get(x)) | inherits(get(x), "spec_tbl_df")
})

## Get the names of the data frames and spec_tbl_df objects
df_names <- all_objects[data_frames]

## Loop through the data frames and spec_tbl_df objects and set their column names to lower case
for (df_name in df_names) {
  df <- get(df_name)
  colnames(df) <- tolower(colnames(df))
  assign(df_name, df)
}

#clean up
rm(all_objects,
   data_frames,
   df_names,
   df_name,
   df)


## create acute code vector
acutes <-c('RHW','RTH','RXQ','RDU','RHM','RHU','R1F','RN5','RN7','RPA',
           'RVV','RWF','RTK','RTP','RA2','RPC','RXC','RYR')

## create calendar and working days calendar
source('helper_files\\calendar_builder.R')

## remove speciality breakdown for now from baseline calculation
## doing this because the baseline isn't being calculated or measured at spec level
## also filtering to just the acutes, will need to do a separate calculation for systems
march_baseline_no_spec <- months_for_march_baseline %>% 
  select(-c(treatment_function_code,
            treatment_function)) %>% 
  filter(der_provider_code %in% acutes) %>% 
  group_by(month_commencing,
           der_provider_code,
           stp_code) %>% 
  summarise(activity = sum(activity)) %>% 
  ungroup()

baseline_excl_march_no_spec <- baseline_excl_march %>% 
  select(-c(treatment_function_code,
            treatment_function)) %>% 
  filter(der_provider_code %in% acutes) %>% 
  group_by(month_commencing,
           der_provider_code,
           stp_code) %>% 
  summarise(activity = sum(activity)) %>% 
  ungroup()

actuals_2022_23_no_spec <- actuals_2022_23 %>% 
  select(-c(treatment_function_code,
            treatment_function)) %>% 
  filter(der_provider_code %in% acutes) %>% 
  group_by(month_commencing,
           der_provider_code,
           stp_code) %>% 
  summarise(activity = sum(activity)) %>% 
  ungroup()

## created trimmed down working days calendar
monthly_working_days <- monthly_working_days %>% 
  select(month_commencing,
         working_days) %>% 
  mutate(month_commencing = as.Date(monthly_working_days$month_commencing))

## merge monthly working days with months for march baseline
march_baseline_no_spec <- left_join(x = march_baseline_no_spec,
                            y = monthly_working_days,
                            by = c('month_commencing'),
                            keep = FALSE)

## check there is reported activity for all providers for all months 
## dq_check <- march_baseline_no_spec %>% 
##  select(der_provider_code) %>% 
##  count(der_provider_code)

## calculate wd adjusted activity for march
march_baseline_no_spec <- march_baseline_no_spec %>% 
  mutate(wd_adjusted = activity/working_days) %>% 
  select(-c(month_commencing,
            activity,
            working_days)) %>% 
  group_by(der_provider_code,
           stp_code) %>% 
  summarise(activity = sum(wd_adjusted)) %>% 
  mutate(activity = activity/3) %>% 
  ungroup() %>% 
  mutate(month_commencing = as.Date('2020-03-01')) %>% 
  select(month_commencing,
         der_provider_code,
         stp_code,
         activity)
           
## merge monthly working days with months for other months of baseline
baseline_excl_march_no_spec <- left_join(x = baseline_excl_march_no_spec,
                                         y = monthly_working_days,
                                         by = c('month_commencing'),
                                         keep = FALSE)

## calculate wd adjusted activity for rest of baseline
baseline_excl_march_no_spec <- baseline_excl_march_no_spec %>% 
  group_by(month_commencing,
           der_provider_code,
           stp_code) %>% 
  mutate(activity = activity/working_days) %>% 
  ungroup() %>% 
  select(-c(working_days))

## create single baseline dataframe
baseline_no_spec_wd_adjusted <- union(x=baseline_excl_march_no_spec,
                                      y=march_baseline_no_spec) 

## create ambition dataframe
ambition <- baseline_no_spec_wd_adjusted %>% 
  mutate(activity = activity*0.75) %>% 
  mutate(period = 'ambition')

## create a period column for baseline dataframe
baseline_no_spec_wd_adjusted <- baseline_no_spec_wd_adjusted %>% 
  mutate(period = 'baseline')

## merge monthly working days with months for actuals
actuals_2022_23_no_spec <- left_join(x = actuals_2022_23_no_spec,
                                    y = monthly_working_days,
                                    by = c('month_commencing'),
                                    keep = FALSE)

## calculate wd adjusted activity for 22/23
actual_wd_adjusted <- actuals_2022_23_no_spec %>% 
  group_by(month_commencing,
           der_provider_code,
           stp_code) %>% 
  mutate(activity = activity/working_days) %>% 
  ungroup() %>% 
  select(-c(working_days)) %>% 
  mutate(period = 'actual')

## bind the three dataframes together to create the system overall dataframe
all_no_spec <- union(x = baseline_no_spec_wd_adjusted,
                     y = actual_wd_adjusted)

all_no_spec <- union(x = all_no_spec,
                     y = ambition)

## need to change month_commencing to just the month
all_no_spec <- left_join(x = all_no_spec,
                 y = calendar,
                 by = c('month_commencing'='date'),
                 keep = FALSE) %>% 
  select(c(month_name_short,
           fin_month,
           der_provider_code,
           stp_code,
           activity,
           period))

## replace codes with names
all_no_spec <- left_join(x = all_no_spec,
                y = lkup_provider_names,
                by = c('der_provider_code' = 'provider_code'),
                keep = FALSE) %>% 
  select(c(month_name_short,
           fin_month,
           provider_short_name,
           stp_code,
           activity,
           period))

all_no_spec <- left_join(x = all_no_spec,
               y = lkup_system_names,
               by = c('stp_code'),
               keep = FALSE) %>% 
  select(c(fin_month,
           month_name_short,
           provider_short_name,
           short_name,
           activity,
           period)) %>% 
  rename(provider = provider_short_name,
         icb = short_name,
         month = month_name_short) 

## reshape to work with my brain
wide_no_spec <- all_no_spec %>% 
  pivot_wider(names_from = period,
              values_from = activity) %>% 
  mutate(variance = actual-ambition) %>% 
  arrange(icb,
          provider,
          fin_month)

## create a variance dataframe to append to the main one
variance <- wide_no_spec %>% select(-c(baseline,
                                 actual,
                                 ambition)) %>% 
  rename(activity = variance) %>% 
  mutate(period = 'variance')
  
## append variance to all_no_spec
all_no_spec <- union(x = all_no_spec,
                     y = variance)

## need to create a date-type variable to allow for continuous x axes 
## for this we will describe the data based on the month of the actual activity
## need to tweak this so that we are not locked into 2022/23 going forwards

all_no_spec <- all_no_spec %>% 
  mutate(month_of_actual = case_when(
    fin_month < 10 ~ paste0('2022-',fin_month+3,'-01'),    
    fin_month > 9 ~ paste0('2023-',fin_month-9,'-01'))
         ) %>% 
  mutate(month_of_actual = as.Date(month_of_actual))

#Now that we know the reduction required per working day we can split it into 
#fair shares for the specialties. For that we need to know what proportion of the
#total monthly activity each specialty accounts for in each provider

acute_2022_23 <- actuals_2022_23 %>%
  filter(der_provider_code %in% acutes)

totals_2022_23 <- actuals_2022_23_no_spec %>% 
  rename(total_activity = activity)

proportions <- left_join(x = acute_2022_23,
                y = totals_2022_23,
                by = c('month_commencing',
                       'der_provider_code',
                       'stp_code'),
                keep = FALSE)

proportions <- left_join(x = proportions,
                         y = lkup_provider_names,
                         by = c('der_provider_code' = 'provider_code'),
                         keep = FALSE) %>% 
  select(c(month_commencing,
           treatment_function_code,
           treatment_function,
           provider_short_name,
           stp_code,
           activity,
           total_activity,
           working_days))

proportions <- left_join(x = proportions,
                         y = lkup_system_names,
                         by = c('stp_code' = 'stp_code'),
                         keep = FALSE) %>% 
  select(c(month_commencing,
           treatment_function_code,
           treatment_function,
           provider_short_name,
           short_name,
           activity,
           total_activity,
           working_days)) %>% 
  rename(provider = provider_short_name,
         icb = short_name) 

proportions <- proportions %>% 
  mutate(wd_adjusted_total = total_activity/working_days,
         ambition = wd_adjusted_total*0.75,
         proportion_of_total = activity/total_activity,
         variance = wd_adjusted_total-ambition,
         reduction_per_day = round(variance*proportion_of_total,digits=1))
         #reduction_per_day = ceiling(variance*proportion_of_total))
## using ceiling because you cannot have 0.4 of a patient so rounding to whole numbers makes
## for a more difficult target but also gives a safety buffer

proportion_split_wide <- proportions %>% 
  select(icb,
         provider,
         treatment_function,
         month_commencing,
         reduction_per_day) %>% 
  arrange(icb,
          provider,
          treatment_function,
          month_commencing) %>% 
  pivot_wider(names_from = month_commencing,
              values_from = reduction_per_day) %>% 
  mutate(total = rowSums(across(where(is.numeric)),na.rm=TRUE)) %>% 
  arrange(icb,
          provider,
          desc(total)) %>% 
  mutate(average_needed = round(total/12,digits=1))

## plotting

system_util_line_plot <- all_no_spec %>% 
  filter(period == 'baseline' | period == 'actual'| period == 'ambition') %>% 
  ggplot(aes(x=month_of_actual,
             y=activity,
             color = period))+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 30, 
                                   hjust = 1,
                                   vjust = 1))

system_util_line_plot+
  geom_line()+
  facet_wrap(~ provider,
             scales = 'free_y')

## tables

wide_no_spec %>% 
  select(-c(fin_month))
  kbl(caption = '...') %>% 
  kable_styling(bootstrap_options = 'striped',
                font_size = 12) #%>% 
  #add_header_above(c(" "=column_count-month_count,"Freeze"=month_count-1,"Flex"=1)) %>% 
  #add_header_above(c(" "=column_count-month_count,"Calendar Month"=month_count)) %>%
#  column_spec(column_count,background = '#D3D3D3') %>% 
 # column_spec(1:2,background = '#CCE3F5', bold=T)
  
  #rmarkdown::render("knit_1.Rmd")
  
  ambition %>% filter(der_provider_code == 'RTK')
  
  
  x <- baseline_no_spec_wd_adjusted %>% filter(der_provider_code == 'RPA'); view(x)