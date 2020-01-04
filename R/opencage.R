place_full_name <- tweets_clean_2019_07_22 %>% 
  filter(is.na(lat)) %>% 
  group_by(place_full_name) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  ungroup()

p_load(opencage)
key <- readr::read_file("opencage.txt")

# temp <- opencage_forward("zero95", key, countrycode = "AU")
# temp$results 
# 
# temp$results$confidence[1]
# temp$results$geometry.lat[1]
# temp$results$geometry.lng[1]
# temp$results$formatted[1]

# solution to keep first (best?) record
# might be worth to keep all of them? with rbind or sth similar?

place_full_name$confidence <- numeric(length = nrow(place_full_name))
place_full_name$formatted <- ""
place_full_name$ext_lat <- numeric(length = nrow(place_full_name))
place_full_name$ext_lon <- numeric(length = nrow(place_full_name))

place_full_name <- readRDS(file = "./data/Twitter/clean/place_full_name.rds")

slice(place_full_name, 17400:17401)

for (i in 17401:19900) {
  temp <- opencage_forward(place_full_name$place_full_name[i], key, countrycode = "AU")
  
  if (is.null(temp$results)) {
    
    place_full_name$confidence[i] <- NA
    place_full_name$formatted[i] <- ""
    place_full_name$ext_lat[i] <- NA
    place_full_name$ext_lon[i] <- NA
    
  } else {
    
    place_full_name$confidence[i] <- temp$results$confidence[1]
    place_full_name$formatted[i] <- temp$results$formatted[1]
    place_full_name$ext_lat[i] <- temp$results$geometry.lat[1]
    place_full_name$ext_lon[i] <- temp$results$geometry.lng[1]
    
  }
  
  Sys.sleep(1)
}

saveRDS(place_full_name, file = "./data/Twitter/clean/place_full_name.rds")
rm(key)

