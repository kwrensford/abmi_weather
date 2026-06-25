#ABMI Covariates

library(dplyr)
library(tidyr)
library(vroom)
library(readr)
library(stringr)
library(ggplot2)
library(camtrapR)
library(lubridate)
library(corrplot)
library(exactextractr)
library(xml2)

#Read in detection data
all_main_reports <- vroom::vroom("data/ABMI EH 2014-2019 Main Reports.csv")

#Read in environmental covariate dataset, buffered and unbuffered
ehs_habitat_unbuffered<-vroom::vroom("data/Point Level Vegetation HF for Kwasi.csv")
ehs_habitat_buffered<-vroom::vroom("data/150m Buffer Vegetation HF for Kwasi.csv")

#Combine buffered and unbuffered dataset
buffered_renamed <- ehs_habitat_buffered %>%
  rename_with(~ paste0(.x, "_buf"),
              .cols = -location)

ehs_habitat <- ehs_habitat_unbuffered %>%
  left_join(buffered_renamed, by = "location")

##Ensure cam coordinates match detection reports##

#Extract cam locations from image reports
camlocations <- all_main_reports %>%
  distinct(location, .keep_all = TRUE)

# Join the two datasets by Station
check_coords <- ehs_habitat %>%
  left_join(camlocations %>% dplyr::select(location, location_id, latitude_cam = latitude, longitude_cam = longitude),
            by = "location")

# Flag mismatches
check_coords <- check_coords %>%
  mutate(
    lat_mismatch = Lat != latitude_cam,
    lon_mismatch = Long != longitude_cam
  )

# View rows with mismatches
filter(check_coords, lat_mismatch | lon_mismatch)

##Replace incorrect coordinates in covariate dataset with correct ones from cam reports
covariates_fixed <- check_coords %>%
  mutate(
    Latitude  = ifelse(lat_mismatch, latitude_cam, Lat),
    Longitude = ifelse(lon_mismatch, longitude_cam, Long)
  ) %>%
  dplyr::select(-latitude_cam, -longitude_cam, -lat_mismatch, -lon_mismatch, -Lat, -Long)

#Canopy cover covariates (I recommend using the 150m buffered canopy cover measure)
names(covariates_fixed)

canopy_cover <- covariates_fixed %>%
  select(project, location, NR, NSR, location_id, Latitude, Longitude, CCDecidR_buf:CCSpruce4_buf)

#Canopy cover covariates are a score of each type of tree's relative canopy cover for each site
#I'd recommend combining into some combined unified canopy cover covariate, probably could just add them all together

#As for elevation, we'll need to extract that with the real coordinates, if you can write a script,
#Marcus can just run it on the real locations along with the weather data

#Write CSV of covariate data for later analyses
write.csv(covariates_fixed, file = "data/abmi_habitat_covars.csv")
write.cvs(canopy_cover, file = "data/abmi_canopycover")

