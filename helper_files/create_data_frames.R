## created trimmed down working days calendar
monthly_working_days <- monthly_working_days %>% 
  select(month_commencing,
         month_name_short,
         fin_year,
         working_days) %>% 
  mutate(month_commencing = as.Date(monthly_working_days$month_commencing))

## get overall activity without speciality
actuals_2022_23_no_spec <- actuals_2022_23 %>% 
  select(-c(treatment_function_code,
            treatment_function,
            stp_code)) %>% 
  filter(der_provider_code %in% acutes) %>% 
  group_by(month_commencing,
           der_provider_code) %>% 
  summarise(activity = sum(activity)) %>% 
  ungroup() %>% 
  rename(provider_code = der_provider_code) %>% 
  mutate(period = 'actual')

## merge monthly working days with baseline
baseline <- left_join(x = baseline,
                      y = monthly_working_days,
                      by = c('month_commencing'),
                      keep = FALSE)  %>% 
  rename(working_days_1920 = working_days,
         activity = value)

## create a 2022/23 working days set to join into baseline for comparison calculation
working_days_2223 <- monthly_working_days %>% 
  filter(fin_year == '2022-23') %>% 
  select(month_name_short,
         working_days) %>% 
  rename(working_days_2223 = working_days)

## merge this into baseline frame with a 2022/23 working day set
baseline <- left_join(x = baseline,
                      y = (monthly_working_days %>%
                             filter(fin_year == '2022-23') %>%
                             select(month_name_short,
                                    working_days) %>%
                             rename(working_days_2223 = working_days)),
                      by = c('month_name_short'),
                      keep = FALSE)

## for some reason working day adjustment is made to the baseline and not the actuals year
## calculate wd adjusted activity for rest of baseline
## formula for working day ajusted activity
## (2019-20 value/number of wd 2019-20 month)* number of wd 2022-23 month

## create a dataframe with:
## the actual activity, 
## the baseline adjusted for working days, 
## and the ambition

all_no_spec <- union(x = (baseline %>% 
                            mutate(activity = activity/working_days_1920*working_days_2223) %>% 
                            select(month_commencing,
                                   orgcode,
                                   activity) %>% 
                            rename(provider_code = orgcode) %>% 
                            mutate(period = 'baseline')),
                     y = actuals_2022_23_no_spec)

all_no_spec <- union(x = all_no_spec,
                     y = (baseline %>% 
                            mutate(activity = activity/working_days_1920*working_days_2223) %>% 
                            select(month_commencing,
                                   orgcode,
                                   activity) %>% 
                            rename(provider_code = orgcode)) %>%
                       mutate(activity = activity*0.75) %>% 
                       mutate(period = 'ambition'))

## change the dates shown to the month name and which financial month it is
## this means that we're not splitting activity by year when comparing baselines variances etc.
all_no_spec <- left_join(x = all_no_spec,
                         y = calendar,
                         by = c('month_commencing'='date'),
                         keep = FALSE) %>% 
  select(c(month_name_short,
           fin_month,
           provider_code,
           activity,
           period))

## reshape to work with my brain
wide_no_spec <- all_no_spec %>% 
  pivot_wider(names_from = period,
              values_from = activity) %>% 
  mutate(variance = actual-ambition) %>% 
  arrange(provider_code,
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

all_no_spec <- left_join(x = all_no_spec,
                         y = lkup_provider_names,
                         by = c('provider_code'),
                         keep = FALSE) 

all_no_spec <- left_join(x = all_no_spec,
                         y = lkup_system_names,
                         by = c('stp_code' = 'stp_code'),
                         keep = FALSE) %>% 
  rename(provider = provider_short_name,
         icb = short_name) %>% 
  select(-c(stp_name,
            stp_code,
            provider_code)) %>% 
  mutate(month_of_actual = case_when(
    fin_month < 10 ~ paste0('2022-',fin_month+3,'-01'),    
    fin_month > 9 ~ paste0('2023-',fin_month-9,'-01'))
  ) %>% 
  mutate(month_of_actual = as.Date(month_of_actual))

######################
## Now that we know the overall reduction required we need to split it into 
## fair shares for the specialties. For that we need to know what proportion of the
## total monthly activity each specialty accounts for in each provider

proportions <- left_join(x= actuals_2022_23 %>%
                           filter(der_provider_code %in% acutes) %>% 
                           rename(speciality_activity = activity),
                         y= (actuals_2022_23_no_spec %>% 
                               rename(total_activity = activity)),
                         by = c('month_commencing',
                                c('der_provider_code' = 'provider_code')),
                         keep = FALSE) %>% 
  mutate(proportion_of_total = speciality_activity/total_activity)

## change the month to just the month as before
proportions <- left_join(x = proportions,
                         y = calendar,
                         by = c('month_commencing'='date'),
                         keep = FALSE) %>% 
  select(c(month_name_short,
           fin_month,
           stp_code,
           der_provider_code,
           treatment_function_code,
           treatment_function,
           speciality_activity,
           total_activity,
           proportion_of_total)) %>% 
  rename(provider_code = der_provider_code)

## bind the proportions into the wide table
wide_spec <- left_join(x=proportions,
                       y=wide_no_spec,
                       by = c('month_name_short',
                              'fin_month',
                              'provider_code'),
                       keep = FALSE)

## switch codes to names
wide_spec <- left_join(x = wide_spec,
                       y = lkup_provider_names,
                       by = c('provider_code', 
                              'stp_code'),
                       keep = FALSE) %>% 
  select(c(month_name_short,
           fin_month,
           stp_code,
           provider_short_name,
           treatment_function_code,
           treatment_function,
           speciality_activity,
           total_activity,
           baseline,           
           ambition,
           variance,
           proportion_of_total
  ))

wide_spec <- left_join(x = wide_spec,
                       y = lkup_system_names,
                       by = c('stp_code' = 'stp_code'),
                       keep = FALSE) %>% 
  select(c(month_name_short,
           fin_month,
           short_name,
           provider_short_name,
           treatment_function_code,
           treatment_function,
           speciality_activity,
           total_activity,
           baseline,           
           ambition,
           variance,
           proportion_of_total
  )) %>% 
  rename(provider = provider_short_name,
         icb = short_name)

## some of these specialities do such a small proportion of the trust's overall activity that
## their "fair share" would be less than 1% of the trust's variance. 
## what if we limited the requirement to just those specialities that did either
## a minimum of 1% of the trust's total monthly activity 
## OR see at least 2000 patients per month on average 
## AND activity has been reported against at least 6 out of the 12 months of data

larger_spec_id <- left_join(x = (wide_spec %>% 
                                   select(icb,
                                          provider,
                                          treatment_function_code,
                                          speciality_activity) %>% 
                                   group_by(icb,
                                            provider,
                                            treatment_function_code) %>% 
                                   summarise(speciality_activity = sum(speciality_activity)) %>% 
                                   ungroup()),
                            y = (wide_spec %>% 
                              distinct(month_name_short,
                                       provider,
                                       total_activity) %>% 
                              group_by(provider) %>% 
                              summarise(total_activity = sum(total_activity)) %>% 
                              ungroup()),
                            by = 'provider',
                            keep = FALSE) %>% 
  mutate(proportion_of_activity = speciality_activity/total_activity) 

larger_spec_id <- left_join(x = larger_spec_id,
                            y = (wide_spec %>% 
                                   select(provider,
                                          month_name_short,
                                          treatment_function_code,
                                          speciality_activity) %>% 
                                   count(provider,treatment_function_code) %>% 
                                   rename(month_tally = n)),
                            by = c('provider',
                                   'treatment_function_code'),
                            keep = FALSE) %>% 
  mutate(avg_monthly_activity = speciality_activity/month_tally) %>% 
  filter(month_tally >8 & 
           (proportion_of_activity >= 0.01|avg_monthly_activity >= 2000)) %>% 
  mutate(large_spec_id = paste0(provider,treatment_function_code))

larger_specialities <- wide_spec %>% 
  filter((paste0(provider,treatment_function_code))%in%larger_spec_id$large_spec_id)


annual_activity_proportion <- larger_spec_id %>% 
  group_by(icb,
           provider) %>% 
  summarise(cohort_activity = sum(proportion_of_activity))

## look at what proportion of the >1% or 2000 activities is done by each TFC
larger_specialities <- left_join(x= larger_specialities,
                                 y= (larger_specialities %>% 
                                       group_by(month_name_short,
                                                fin_month,
                                                icb,
                                                provider) %>% 
                                       summarise(cohort_activity = sum(speciality_activity)) %>% 
                                       ungroup()),
                                 by= c('month_name_short',
                                       'fin_month',
                                       'icb',
                                       'provider'),
                                 keep= FALSE) %>% 
  mutate(cohort_proportion = speciality_activity/cohort_activity,
         monthly_reduction = variance*cohort_proportion) %>% 
  arrange(icb,
          provider,
          fin_month,
          treatment_function)

## add number of working days per month for the year where the activity was recorded
## calculate speciality activity per working day
## calculate the variance per wd
## calculate the portion of the daily variance each speciality accounts for

larger_specialities_monthly <- left_join(x= larger_specialities,
                                         y= (monthly_working_days %>% 
                                               filter(fin_year == '2022-23') %>%
                                               select(c(month_name_short,
                                                        working_days)) %>%
                                               rename(wd_2223 = working_days)),
                                         by= 'month_name_short',
                                         keep = FALSE) %>% 
  mutate(activity_per_wd = speciality_activity/wd_2223,
         variance_per_wd = variance/wd_2223,
         wd_variance_by_spec = variance_per_wd*cohort_proportion) %>% 
  select(icb,
         provider,
         month_name_short,
         treatment_function_code,
         treatment_function,
         total_activity,
         baseline,
         ambition,
         variance,
         variance_per_wd,
         cohort_activity,
         cohort_proportion,
         monthly_reduction,
         wd_2223,
         speciality_activity,
         activity_per_wd,
         wd_variance_by_spec)

## for simplicity the df below gives the average activity per working day across the year
## this if statement is in because the first iteration of this report is being created before march freeze is in
if(max(actuals_2022_23$month_commencing) <= '2023-03-01'){
  total_wd_2223 <- working_days_2223 %>% 
    filter(month_name_short != 'Mar') 
  month_count <- nrow(total_wd_2223)
  total_wd_2223 <- as.numeric(sum(total_wd_2223$working_days_2223))
  }else{
    month_count <- nrow(working_days_2223)
    total_wd_2223 <- as.numeric(sum(working_days_2223$working_days_2223))
    }

larger_specialities_average <- larger_specialities %>% 
  select(icb,
         provider,
         treatment_function_code,
         treatment_function,
         total_activity,
         baseline,
         ambition,
         variance,
         cohort_activity,
         speciality_activity) %>% 
  group_by(icb,
           provider,
           treatment_function_code,
           treatment_function) %>% 
  summarise(total_activity = sum(total_activity),
            baseline = sum(baseline),
            ambition = sum(ambition),
            variance = sum(variance),
            cohort_activity = sum(cohort_activity),
            speciality_activity = sum(speciality_activity)) %>% 
  ungroup() %>% 
  mutate(cohort_proportion = speciality_activity/cohort_activity,
         avg_monthly_activity = speciality_activity/month_count,
         avg_monthly_variance = variance/month_count,
         share_of_monthly_variance = avg_monthly_variance*cohort_proportion,
         avg_activity_per_wd = speciality_activity/total_wd_2223,
         variance_per_wd = variance/total_wd_2223,
         share_of_daily_variance = variance_per_wd*cohort_proportion) %>% 
  arrange(icb,
          provider,
          desc(share_of_daily_variance)) %>% 
  select(icb,
         provider,
         treatment_function,
         avg_monthly_activity,
         share_of_monthly_variance,
         avg_activity_per_wd,
         share_of_daily_variance) %>%
  mutate(across(c(avg_monthly_activity,
                  share_of_monthly_variance,
                  avg_activity_per_wd,
                  share_of_daily_variance),
                ~round(.,1)))

######################################################################### 
wide_year <- wide_no_spec %>% 
  group_by(provider_code) %>% 
  summarise(across(c(baseline,actual,ambition), ~ sum(.,na.rm=TRUE))) %>% 
  mutate(variance = actual-ambition,
         perc_variance = actual/ambition-1,
         variance_per_wd = variance/total_wd_2223)

## prep a dataframe to put into a table showing the specialities ordered by reduction

reduction_per_day_per_spec <- larger_specialities_monthly %>% 
  select(month_name_short,
         icb,
         provider,
         treatment_function,
         wd_variance_by_spec) %>% 
  pivot_wider(names_from = month_name_short,
              values_from = wd_variance_by_spec) %>% 
  arrange(icb,
          provider,
          desc(Apr))

## prep a dataframe to put into a table showing the daily reduction required per provider  

wide_no_spec <- all_no_spec %>% 
  pivot_wider(names_from = period,
              values_from = activity) %>% 
  mutate(variance = actual-ambition) %>% 
  arrange(provider,
          fin_month)

wide_no_spec <- left_join(x= wide_no_spec,
                          y= (monthly_working_days %>% 
                                filter(fin_year == '2022-23') %>%
                                select(c(month_name_short,
                                         working_days)) %>%
                                rename(wd_2223 = working_days)),
                          by= 'month_name_short',
                          keep = FALSE) %>% 
  mutate(activity_per_wd = actual/wd_2223,
         ambition_per_wd = ambition/wd_2223,
         variance_per_wd = variance/wd_2223) %>%   
  select(month_name_short,
         fin_month,
         icb,
         provider,
         baseline,
         actual,
         ambition,
         variance,
         activity_per_wd,
         ambition_per_wd,
         variance_per_wd)

reduction_per_day <- wide_no_spec %>% 
  select(month_name_short,
         icb,
         provider,
         variance_per_wd) %>% 
  pivot_wider(names_from = month_name_short,
              values_from = variance_per_wd) %>% 
  arrange(icb,
          provider,
          desc(Apr))

## create a data frame giving the full year view and to show the overall percentage variance from the goal

wide_year <- wide_no_spec %>% 
  group_by(icb, provider) %>% 
  summarise(across(c(baseline,actual,ambition), ~ sum(.,na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(variance = actual-ambition,
         percent_vs_2223 = variance/actual,
         variance_per_wd = variance/total_wd_2223)
