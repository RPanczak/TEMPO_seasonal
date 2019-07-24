# #################################################
# #################################################
# Twitter - data preparation HPC

# messaging file
fileConn <- file("/RDS/Q0786/data/Twitter/clean/tweets_proc_2019_07_22.log", "w")
writeLines(paste(Sys.time(), "Job started"), fileConn)

set.seed(12345)
library(pacman)
p_load(rgdal, rgeos, dplyr, tidyr, magrittr, stringr, anytime, lubridate) #, summarytools)

isUnique <- function(vector){
  return(!any(duplicated(vector)))
}

# #################################################
# #################################################
# Geo data
STE <- readOGR("/RDS/Q0786/data/geo/1270055001_ste_2016_aust_shape/STE_2016_AUST.shp")
STE$AREASQKM16 <- NULL
STE <- spTransform(STE, CRS("+init=epsg:4326"))

# buffer 0.5 degrees prepared in ArcGIS
STE_b_05 <- readOGR("/RDS/Q0786/data/geo/1270055001_ste_2016_aust_shape/STE_2016_AUST_B05.shp")
STE_b_05 <- spTransform(STE_b_05, CRS("+init=epsg:4326"))

# Data  
tweets_raw <- readRDS("/RDS/Q0786/data/Twitter/raw/tweets_raw.rds")

# #################################################
# #################################################

# Cleand empty rows 
tweets_proc_2019_07_22 <- tweets_raw[!apply(is.na(tweets_raw) | tweets_raw == "", 1, all), ]
# Cleand duplis
tweets_proc_2019_07_22 <- tweets_proc_2019_07_22[!duplicated(tweets_proc_2019_07_22), ]

rm(tweets_raw)
gc()

### Rename variables
tweets_proc_2019_07_22 %<>% 
  rename(lat = coordinates_coordinates_1,
         lon = coordinates_coordinates_0)

# #################################################
### IDs

#### Fixing user id

# length(unique(tweets_proc_2019_07_22$user_id_str))
# length(unique(tweets_proc_2019_07_22$user_screen_name))

tweets_proc_2019_07_22 %<>% 
  mutate(UID = group_indices_(tweets_proc_2019_07_22, .dots=c("user_id_str", "user_screen_name")))

#### Fixing tweet id 

# isUnique(tweets_proc_2019_07_22$id_str)
# length(tweets_proc_2019_07_22$id_str)
# length(unique(tweets_proc_2019_07_22$id_str))

# tweets_proc_2019_07_22 %>% select(id_str, created_at, place_full_name) %>% filter(id_str == "1.00028e+18")

# #################################################
### Fixing time (`created_at` variable)

tweets_proc_2019_07_22 %<>% 
  mutate(created_at = str_sub(created_at, 4)) %>% # removing front of the string with day of the week
  mutate(created_at = str_trim(created_at)) %>% # trim white space from both sides
  tidyr::separate(created_at, c("t1", "t2", "t3", "t4", "t5"), sep = " ", remove = FALSE) # separate to components

# time zone seems to be the same, so ignored
# summarytools::freq(tweets_proc_2019_07_22$t4, order = "freq")#, report.nas = TRUE) 

# creating POSIXct object
tweets_proc_2019_07_22 %<>% 
  mutate(created_at = anytime(paste(t2, t1, t5, t3), tz="UTC", asUTC = TRUE))

# tweets_proc_2019_07_22 %>% 
#   select(created_at, t5, t1, t2, t3)

# components not needed any longer
tweets_proc_2019_07_22 %<>% 
  select(-(t1:t5))

# summary(tweets_proc_2019_07_22$created_at)

# #################################################
### Geography

#### `geo` variable
tweets_proc_2019_07_22 %<>% 
  mutate(geo = str_replace(geo, fixed('{"type":"Point","coordinates":['), "")) %>% 
  mutate(geo = str_replace(geo, fixed(']}'), "")) %>% 
  tidyr::separate(geo, c("geo_lat", "geo_lon"), sep = ",", convert = TRUE)

geo1 <- tweets_proc_2019_07_22 %>% 
  filter(!is.na(geo_lat)) %>% 
  group_by(geo_lat, geo_lon) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  ungroup()

geo1_sp <- SpatialPointsDataFrame(coords = geo1[ , c("geo_lon", "geo_lat")],
                                  data = geo1, 
                                  proj4string = CRS("+init=epsg:4326"))

saveRDS(geo1_sp, file = "/RDS/Q0786/data/Twitter/clean/geo1_sp.rds")
# writeOGR(geo1_sp, dsn = "/RDS/Q0786/data/Twitter/clean/", layer= "geo1_sp", driver = "ESRI Shapefile", overwrite_layer = TRUE) 

tweets_proc_2019_07_22 %<>% 
  mutate(orig_lat = lat) %>% 
  mutate(orig_lon = lon) %>% 
  mutate(lat = ifelse(is.na(geo_lat), lat, geo_lat)) %>% 
  mutate(lon = ifelse(is.na(geo_lat), lon, geo_lon)) 

#### Twitter coordinates

geo2 <- tweets_proc_2019_07_22 %>% 
  filter(!is.na(lat)) %>% 
  group_by(lat, lon) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  ungroup()

geo2_sp <- SpatialPointsDataFrame(coords = geo2[ , c("lon", "lat")],
                                  data = geo2, 
                                  proj4string = CRS("+init=epsg:4326"))

saveRDS(geo2_sp, file = "/RDS/Q0786/data/Twitter/clean/geo2_sp.rds")
# writeOGR(geo2_sp, dsn = "/RDS/Q0786/data/Twitter/clean/", layer= "geo2_sp", driver = "ESRI Shapefile", overwrite_layer = TRUE) 


##### Tweets **inside** Oz
geo2_inside_sp <- SpatialPointsDataFrame(coords = geo2[ , c("lon", "lat")],
                                         data = geo2, 
                                         proj4string = CRS("+init=epsg:4326"))


geo2_inside_sp <- geo2_inside_sp[!is.na(sp::over(geo2_inside_sp, as(STE_b_05, "SpatialPolygons"))),]

saveRDS(geo2_inside_sp, file = "/RDS/Q0786/data/Twitter/clean/geo2_inside_sp.rds")
# writeOGR(geo2_inside_sp, dsn = "/RDS/Q0786/data/Twitter/clean/", layer= "geo2_inside_sp", driver = "ESRI Shapefile", overwrite_layer = TRUE) 


##### Tweets **outside**  Oz

geo2_outside_sp <- gDifference(geo2_sp, geo2_inside_sp)

geo2_outside_sp <- as.data.frame(geo2_outside_sp@coords) %>% 
  rename(lon = x, lat = y) %>% 
  left_join(geo2)

geo2_outside_sp <- SpatialPointsDataFrame(coords = geo2_outside_sp[ , c("lon", "lat")],
                                          data = geo2_outside_sp, 
                                          proj4string = CRS("+init=epsg:4326"))

saveRDS(geo2_outside_sp, file = "/RDS/Q0786/data/Twitter/clean/geo2_outside_sp.rds")
# writeOGR(geo2_outside_sp, dsn = "/RDS/Q0786/data/Twitter/clean/", layer= "geo2_outside_sp", driver = "ESRI Shapefile", overwrite_layer = TRUE) 


#### `place_full_name`

# place_full_name <- tweets_proc_2019_07_22 %>% 
#   filter(is.na(lat)) %>% 
#   group_by(place_full_name) %>% 
#   summarise(freq = n()) %>% 
#   arrange(desc(freq)) 

# summarytools::freq(place_full_name$place_full_name, order = "freq", report.nas = TRUE) 

complete_place_full_name <- tweets_proc_2019_07_22 %>% 
  filter(is.na(lat)) %>% 
  filter(place_place_type != "country") %>% 
  filter(place_place_type != "admin") %>% 
  group_by(place_full_name, place_place_type) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  ungroup()

saveRDS(complete_place_full_name, file = "/RDS/Q0786/data/Twitter/clean/complete_place_full_name.rds")

# #################################################
### Fixing timezones

complete_tweets_geo_tz <- tweets_proc_2019_07_22 %>% 
  group_by(lat, lon) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(!is.na(lat)) %>% 
  select(-count)

saveRDS(complete_tweets_geo_tz, file = "/RDS/Q0786/data/Twitter/clean/complete_tweets_geo_tz.rds")

saveRDS(tweets_proc_2019_07_22, file = "/RDS/Q0786/data/Twitter/clean/tweets_proc_2019_07_22.rds")


# #################################################

writeLines(paste(Sys.time(), "Finished"), fileConn)
close(fileConn)
