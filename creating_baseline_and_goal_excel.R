library(writexl)

baseline <- read_csv("data/baseline_from_national_tables.csv", 
                     col_types = cols(month_commencing = col_date(format = "%d/%m/%Y")))

## set up lookups
source('helper_files\\lookups.R')

## all to lower
source('helper_files\\all_to_lower.R')

## create calendar and working days calendar
source('helper_files\\calendar_builder.R')

## created trimmed down working days calendar
monthly_working_days <- monthly_working_days %>% 
  select(month_commencing,
         month_name_short,
         fin_year,
         working_days) %>% 
  mutate(month_commencing = as.Date(monthly_working_days$month_commencing))

## merge monthly working days with baseline
baseline <- left_join(x = baseline,
                      y = monthly_working_days,
                      by = c('month_commencing'),
                      keep = FALSE)  %>% 
  rename(working_days_1920 = working_days,
         activity = value)

## create working day adjusted baseline 
wd_adj_baseline <- left_join(x = baseline,
                      y = (monthly_working_days %>%
                             filter(fin_year == '2023-24') %>%
                             select(month_name_short,
                                    working_days) %>%
                             rename(working_days_2324 = working_days)),
                      by = c('month_name_short'),
                      keep = FALSE) %>% 
  mutate(adj_activity = activity/working_days_1920*working_days_2324) %>% 
  mutate(target = adj_activity*.075)

write_xlsx(wd_adj_baseline,
           paste0("Output/baseline_and_goal.xlsx"))
