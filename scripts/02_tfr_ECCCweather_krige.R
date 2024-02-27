######################################################
############ Moose Tag Fill Rates Project ############ 
######################################################
### Understanding what factors impact hunter success rates and developing
### a model to predict success rates for upcoming hunting seasons

######################################################
### Script to krige daily weather data to create daily temperature and
### precipitation estimates at the WMU scale

library(tidyverse)
source("set_up.R")

### import extracted ECCC weather data
weather <- readRDS("data/processed/ECCweather.rds")

### import simplified WMU shapefile
wmu_weather <- readRDS("data/processed/wmu_shp_simp.rds") 

### load weather station locations and create shapefile
stn.id<- read.csv(paste0("data/raw/Ontario_Weather_StationsFin.csv"), header=T, sep=",") ##This is a cleaned and revised file of weather stations with Manitoba and Quebec stations included to remove issues with 
stations <- stn.id[!is.na(stn.id$lon) |
                     !is.na(stn.id$lat),] # No coords
sp::coordinates(stations) <- ~lon+lat
sp::proj4string(stations) <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
stations <- sf::st_as_sf(stations)
stations <- sf::st_transform(stations, sf::st_crs(wmu_weather)) %>%
  select(climate_id)

### function for creating wmu-specific daily temperature estimates
get_temp <- function(polys,
                     date){
  ### specify wmu and date you want predictions for
  pred <- tibble(Tag_WMU = polys$WMU, Date = date)
  
  ### extract temperature data for specified date
  temper <- weather %>%
    filter(date_time == date) %>%
    drop_na(Mean_temp) %>%
    ### join station spatial data
    left_join(stations) %>%
    st_as_sf()
  
  # Krige over space
  int_values <- gstat::krige(Mean_temp~1, 
                             locations = temper, 
                             newdata = polys)  
  
  # Summary
  outdat <- st_drop_geometry(int_values)
  
  # Return prediction for WMU
  pred <- pred %>%
    mutate(pred = outdat$var1.pred)
  return(pred)
}
### function for creating wmu-specific daily precipitation estimates
get_precip <- function(polys,
                       date){
  
  # Get snow data and fix stuff
  pred <- tibble(Tag_WMU = polys$WMU, Date = date)
  
  precip <- weather %>%
    filter(date_time == date) %>%
    drop_na(Total_Precip_mm) %>%
    left_join(stations) %>%
    st_as_sf()
  
  
  # Krige over space
  int_values <- gstat::krige(Total_Precip_mm~1, 
                             locations = precip, 
                             newdata = polys)  
  
  # Summary
  outdat <- st_drop_geometry(int_values)
  
  # Return sp
  pred <- pred %>%
    mutate(pred = outdat$var1.pred)
  return(pred)
}
### Run temperature function for each unique WMU-date
temper_days <- unique(weather$date_time) %>%
  map_dfr(function(x) {
    get_temp(polys = wmu_weather,
             date = x)
  }
  )
### Run precipitation function for each unique WMU-date
precip_days <- unique(weather$date_time) %>%
  map_dfr(function(x) {
    get_precip(polys = wmu_weather,
               date = x)
  }
  )

saveRDS(temper_days,"data/processed/WMU_temp_eccc.rds")
saveRDS(precip_days,"data/processed/WMU_precip_eccc.rds")
