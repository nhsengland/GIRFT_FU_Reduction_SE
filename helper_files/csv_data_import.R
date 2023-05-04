## Import data
actuals_2022_23 <- read_csv("data/actuals_2022_23.csv", 
                            col_types = cols(Month_commencing = col_date(format = "%d/%m/%Y"), 
                                             Treatment_Function_Code = col_character()))

baseline <- read_csv("data/baseline_from_national_tables.csv", 
                     col_types = cols(month_commencing = col_date(format = "%d/%m/%Y")))

  
if (max(actuals_2022_23$Month_commencing) <= '2023-03-01') {
  actuals_2022_23 <- actuals_2022_23 %>% filter(!Month_commencing =='2023-03-01')
  baseline <- baseline %>% filter(!month_commencing=='2020-03-01')}
          
  
  



