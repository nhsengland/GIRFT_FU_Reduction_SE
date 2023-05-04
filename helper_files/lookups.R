## set up lookups
lkup_provider_names <- read_csv("lookups/provider_short_names.csv")
lkup_system_names <- read_csv("lookups/system_names.csv")
lkup_tfc_list <- read_csv("lookups/tfc_list.csv")

## create acute code vector
acutes <-c('RHW','RTH','RXQ','RDU','RHM','RHU','R1F','RN5','RN7','RPA',
           'RVV','RWF','RTK','RTP','RA2','RPC','RXC','RYR')