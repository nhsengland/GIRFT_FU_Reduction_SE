library(jsonlite)

bank_holidays <- "https://www.gov.uk/bank-holidays.json"
bank_holidays <- fromJSON(bank_holidays)

bank_holidays <- bank_holidays[["england-and-wales"]]
bank_holidays <- bank_holidays$events

