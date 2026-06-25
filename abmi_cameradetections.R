##ABMI Weather Data Detection Data and Cam Location Data

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


# Camera Reports
all_main_reports <- vroom::vroom("data/ABMI EH 2014-2019 Main Reports.csv")
all_image_reports <- vroom::vroom("data/ABMI EH 2014-2019 Image Reports.csv")

#Extract locations for all cameras
camlocations <- all_main_reports %>%
  distinct(location, .keep_all = TRUE)

##Create column for year of report
all_main_reports$year <- lubridate::year(all_main_reports$image_date_time)
all_image_reports$year <- lubridate::year(all_image_reports$image_date_time)

##Create column for month
all_main_reports$month <- lubridate::month(all_main_reports$image_date_time)
all_image_reports$month <- lubridate::month(all_image_reports$image_date_time)

##Filter reports to 2015-2019 range (2014 and 2020 are incomplete datasets so we're ignoring for now)
study_image_reports<-all_image_reports %>%
  filter(year > 2014 & year < 2020)

study_main_reports<-all_main_reports %>%
  filter(year > 2014 & year < 2020)

##Filter to Apr - July for closure

study_main_reports<-study_main_reports %>%
  filter(month > 3 & month < 8)

study_image_reports<-study_image_reports %>%
  filter(month > 3 & month < 8)

#Species List
species_list <- unique(study_main_reports$species_common_name)
species_list 

##Filter to study species of interest (feel free to explore other species!)
thesis_main_reports <- study_main_reports %>%
  filter(species_common_name == "Black Bear"
         | species_common_name == "Mule Deer"
         | species_common_name == "Snowshoe Hare"
         | species_common_name == "Red Squirrel")

#Generate delta time (intervals between detection)
study_intervals <- thesis_main_reports %>%
  arrange(species_common_name, location, image_date_time) %>%
  group_by(species_common_name, location) %>%
  mutate(delta_time = as.numeric(difftime(image_date_time, lag(image_date_time), units = "mins"))) %>%
  ungroup()


#Set independence threshold to 5 minutes
threshold <- 5

independent_detections <- study_intervals %>%
  arrange(species_common_name, location, image_date_time) %>%
  group_by(species_common_name, location) %>%
  mutate(
    independent = if_else(
      is.na(delta_time) | delta_time > threshold,
      TRUE,
      FALSE
    )
  ) %>%
  ungroup()

independent_events <- independent_detections %>%
  filter(independent)

independent_events %>%
  group_by(species_common_name) %>%
  summarize(
    n = n()
  )

thesis_main_reports <- independent_events

#Write detection data to csv
write.csv(thesis_main_reports, file = "data/abmi_weather_detections.csv")


