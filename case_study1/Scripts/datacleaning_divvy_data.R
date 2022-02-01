###############################################################################################################
# Author: Bruce Peng
# Date: 10/11/2021
# Email: dpen466@aucklanduni.co.nz
# Github: https://github.com/bp30
# Description: This script contains data cleaning procedure for the Cyclistics bike 
##             sharing data for Case study 1 of the Google date Analyst course on Coursera. See accompanying readme.txt
##             for a more indepth description of the project goal and 

# Data description: Data was obtained from open sourced webpage maintained by Motivate International Inc
##                  a Chicago based bike sharing company. Anonymised monthly trip data are released on a monthly basis.
##                  Only the trialing 12 months(i.e., from 1/10/2020 to 31/10/2021) are included in this project 
##                  More information can be found at https://www.divvybikes.com/system-data
##
# Data source: https://divvy-tripdata.s3.amazonaws.com/index.html
# Data license information: https://www.divvybikes.com/data-license-agreement
###############################################################################################################


# Load necessary packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, stringr)

# set directory
setwd('C:/Users/bruce/My Drive/Phd (1)/Data science/Cousera/Google_Analytics_Certificate/Course8/google_certificate_case_studies/case_study1/Data/Trailing_12months/')

# Combine the trailing 12 months ride sharing data files
file <- list.files(pattern = '*tripdata.csv')
combined_data <- tibble()
for (x in seq(1:length(file))) {
  data <- read_csv(file[x])
  combined_data <- rbind(combined_data, data)
}

# check and remove ride_id duplicates (there are a total of 209 duplicates)
sum(duplicated(combined_data$ride_id))
duplicate_data <- combined_data %>% filter(ride_id %in% combined_data$ride_id[duplicated(combined_data$ride_id)])
View(duplicate_data)

# Check if station name strings are consistent throughout for both start and end stations.
View(data.frame(unique(combined_data$start_station_name)))
View(data.frame(unique(combined_data$end_station_name)))

# Remove duplicates, incorrect station name, trips for testing or maintenance/repairs, convert date and factor variables to the correct format and create a year_month variable, 
# retain only positive value for duration and those with <= 5000 minutes, as well as convert strings to title format to be consistent throughout. Lastly, remove long and lat variables
cleaned_data <- combined_data %>% 
                      filter(!duplicated(ride_id) |
                               !start_station_name %in% c('DIVVY CASSETTE REPAIR MOBILE STATION',
                                                          'WATSON TESTING - DIVVY', 
                                                          'HUBBARD ST BIKE CHECKING (LBS-WH-TEST)') |
                               !end_station_name %in% c('DIVVY CASSETTE REPAIR MOBILE STATION',
                                                        'WATSON TESTING - DIVVY',
                                                        'HUBBARD ST BIKE CHECKING (LBS-WH-TEST)')) %>% 
                      mutate(started_at = as_datetime(started_at),
                             ended_at = as_datetime(ended_at),
                             year = format(as_date(started_at), "%Y"),
                             month = format(as_date(started_at), "%m"),
                             day = format(as_date(started_at), "%d"),
                             day_of_week = weekdays(started_at),
                             year_month = paste0(year, "_", month),
                             ride_length_min = as.numeric(round(difftime(ended_at, started_at, units = 'mins'), 2)),
                             rideable_type = as.factor(rideable_type), 
                             member_casual = as.factor(member_casual),
                             start_station_name = str_to_title(start_station_name),
                             end_station_name = str_to_title(end_station_name)) %>% 
                     filter(between(ride_length_min, 0, 5000)) %>% 
                     select(-c(start_lat, start_lng, end_lat, end_lng))


## ensure no more duplicates in ride id and testing stations have been removed from start_station_name and end_station_name
sum(duplicated(cleaned_data$ride_id))
sum(cleaned_data$start_station_name %in% c('DIVVY CASSETTE REPAIR MOBILE STATION',
                                           'WATSON TESTING - DIVVY',
                                           'HUBBARD ST BIKE CHECKING (LBS-WH-TEST)'))
sum(cleaned_data$end_station_name %in% c('DIVVY CASSETTE REPAIR MOBILE STATION',
                                         'WATSON TESTING - DIVVY',
                                         'HUBBARD ST BIKE CHECKING (LBS-WH-TEST)'))
sum(!between(cleaned_data$ride_length_min, 0, 5000))


# Ensure station_id and station_name have unique pairings
start_station_pairing <- unique(cleaned_data[c('start_station_name', 'start_station_id')])
end_station_pairing <- unique(cleaned_data[c('end_station_name', 'end_station_id')])
View(start_station_pairing); View(end_station_pairing)
# There are stations paired with two or more station id in both start and end stations (583 duplicated pairings in start_station and 580 in end_station)
# We will need to check with stakeholder on why this is the case
sum(duplicated(start_station_pairing$start_station_name))
sum(duplicated(end_station_pairing$end_station_name))
duplicate_start_station <- start_station_pairing %>% 
                              filter(start_station_name %in% start_station_name[duplicated(start_station_name)])
duplicate_end_station <- end_station_pairing %>% 
                              filter(end_station_name %in% end_station_name[duplicated(end_station_name)])

# check if there is anything else amiss
View(cleaned_data); summary(cleaned_data); str(cleaned_data)
## There is some missing start and end locations but these may not be of concern for now

# the cleaned data set contains 5,763,084 cases

# save the cleaned data file
write_csv(x = cleaned_data, '2021_10_divvy_data_TT12.csv')
