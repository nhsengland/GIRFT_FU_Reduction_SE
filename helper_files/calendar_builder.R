library(tidyverse)
library(jsonlite)

bank_holiday <- "https://www.gov.uk/bank-holidays.json"
bank_holiday <- fromJSON(bank_holiday)

bank_holiday <- bank_holiday[["england-and-wales"]]
bank_holiday <- bank_holiday$events
bank_holiday <- bank_holiday$date
bank_holiday <- data.frame(date=bank_holiday)
bank_holiday <- rbind(data.frame(date=c('2017-01-02',
                                        '2017-04-14',
                                        '2017-04-17',
                                        '2017-05-01',
                                        '2017-05-29',
                                        '2017-08-28',
                                        '2017-12-25',
                                        '2017-12-26')),
                      bank_holiday)
bank_holiday <- as.Date(bank_holiday$date)


## creates a list of the days between first April 2017 and 31 March 2024
date <- seq(as.Date('2017-01-01'), as.Date('2025-12-26'), by='days')

## bind the main dates into a data frame
calendar <-  data.frame(date)

## add useful calculated columns
calendar <- calendar %>% 
  mutate(
    day_num = day(date),
    day_name = as.character(wday(date,getOption('lubridate.week.start',1))),
    month_num = month(date),
    year_num = year(date),
    month_name = as.character(month(date,label = TRUE)),
    month_name_short = as.character(month(date,label = TRUE,abbr=TRUE)),
    fin_year = case_when(
      month(date)>=4 ~ paste0(as.character(year(date)),'-',substr(year(date)+1,3,4)),
      month(date)<=3 ~ paste0(as.character(year(date)-1),'-',substr(year(date),3,4))
      ),
    fin_month = case_when(
      month(date)<=3 ~ month(date)+9,
      month(date)>=4 ~ month(date)-3
      ),
    working_day = case_when(
      (wday(date,
            label=FALSE,
            getOption('lubridate.week.start',1)) %in% c(1,7)) 
        | date %in% bank_holiday ~ 'no',
      (wday(date,
            label=FALSE,
            getOption('lubridate.week.start',1)) %in% c(2,3,4,5,6)) 
        & !(date %in% bank_holiday) ~ 'yes'
      )
    )
## tidy up the vectors as don't need them now
rm(date,
   bank_holiday)   

## create a separate data frame counting the number of working days per month     
monthly_working_days <- calendar %>%
  filter(working_day == 'yes') %>% 
  select (month_name_short,
          month_num,
          year_num,
          fin_year,
          fin_month) %>% 
  mutate(month_commencing = case_when(
     month_num < 10 ~ paste0(year_num,'-0',month_num,'-01'),
     month_num > 9 ~ paste0(year_num,'-',month_num,'-01'))) %>% 
  count(month_name_short,
        month_num,
        year_num,
        month_commencing,
        fin_year,
        fin_month) %>% 
  mutate(working_days = n) %>% 
  select(-c(n)) %>% 
  arrange(year_num,
          month_num,
          month_commencing)
  
