# TMR data 
# read large csv, clean & save to Rds

set.seed(12345)
options(scipen = 999)

library(dplyr)
library(readr) 
library(janitor) 
library(lubridate) 

austroads <- readRDS(file = "/90days/uqrpancz/TMR/austroads.Rds") %>% 
  rename(traffic_class_code = code)

counts_18 <- read_csv("/90days/uqrpancz/TMR/TMR Traffic Counts 2018.txt") %>%
  clean_names() %>%
  mutate(site_id = as.integer(site_id),
         hour = as.integer(hour),
         date = dmy(dates),
         date_time = dmy_h(paste(dates, hour), tz = "Australia/Brisbane")) %>%
  select(site_id, site_stream, date, hour, date_time, everything(), -dates)

saveRDS(counts_18, file = "/90days/uqrpancz/TMR/counts_18.Rds")

site_class <- counts_18 %>% 
  select(-site_stream, -hour) %>% 
  filter(value > 0) %>% 
  left_join(austroads) %>% 
  group_by(site_id, type) %>% 
  filter(row_number() == 1) %>% 
  group_by(site_id) %>% 
  summarize(Ncats = n())

saveRDS(site_class, file = "/90days/uqrpancz/TMR/site_class.Rds")

site_class_date_time <- counts_18 %>% 
  select(-site_stream, -hour) %>% 
  filter(value > 0) %>% 
  left_join(austroads) %>% 
  group_by(site_id, date_time, type) %>% 
  filter(row_number() == 1) %>% 
  group_by(site_id, date_time) %>% 
  summarize(Ncats = n())

saveRDS(site_class_date_time, file = "/90days/uqrpancz/TMR/site_class_date_time.Rds")
