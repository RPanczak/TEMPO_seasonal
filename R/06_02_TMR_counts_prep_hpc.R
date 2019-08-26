# TMR data 
# read large csv, clean & save to Rds

set.seed(12345)
options(scipen = 999)

library(dplyr) 
library(readr) 
library(janitor) 
library(lubridate) 

counts_18 <- read_csv("/90days/uqrpancz/TMR/TMR Traffic Counts 2018.txt") %>% 
  clean_names() %>%
  mutate(site_id = as.integer(site_id),
         hour = as.integer(hour),
         date = dmy(dates),
         date_time = dmy_h(paste(dates, hour), tz = "Australia/Brisbane")) %>% 
  select(site_id, site_stream, date, hour, date_time, everything(), -dates)

saveRDS(counts_18, file = "/90days/uqrpancz/TMR/counts_18.Rds")