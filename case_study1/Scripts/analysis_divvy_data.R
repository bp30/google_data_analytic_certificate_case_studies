###############################################################################################################
# Author: Bruce Peng
# Date: 10/11/2021
# Email: dpen466@aucklanduni.co.nz
# Github: https://github.com/bp30
# Description: This script contains data analysis for the Cyclistics bike 
##             sharing data for Case study 1 of the Google date Analyst course on Coursera. See accompanying readme.txt
##             for a more in depth description of the project goal and 

# Data description: Data was obtained from open sourced webpage maintained by Motivate International Inc
##                  a Chicago based bike sharing company. Anonymised monthly trip data are released on a monthly basis.
##                  Only the trialing 12 months(i.e., from 1/10/2020 to 31/10/2021) are included in this analysis. 
##                  More information can be found at https://www.divvybikes.com/system-data.
##
# Data source: https://divvy-tripdata.s3.amazonaws.com/index.html
# Data license information: https://www.divvybikes.com/data-license-agreement
###############################################################################################################

# Set directory
setwd('C:/Users/bruce/My Drive/Phd (1)/Data science/Cousera/Google_Analytics_Certificate/Course8/google_certificate_case_studies/case_study1/Data/Trailing_12months/')

# Load necessary packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2)

# Load data and remove ride with 0 duration 
divvy_data <- read_csv("2021_10_divvy_data_TT12.csv") %>% 
                       filter(ride_length_min > 0)

# order the days of the week
divvy_data$day_of_week <- ordered(as.factor(divvy_data$day_of_week), 
                                  levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

###############################################################################################################
# Check the distribution of ride length via member type
## dataset too large to efficiently visualize, I decided to randomly sample 20% of the data to visualize the distribution 
sample_data <- sample_n(divvy_data, dim(divvy_data)[1] * 0.2)
distribution_plot <- sample_data %>% ggplot(aes(x = member_casual, y = ride_length_min, color = member_casual)) +
                                            geom_violin() +
                                            coord_flip() +
                                            geom_boxplot(width = .2) +
                                            geom_point(position = position_jitter(seed = 1, width = 0.2)) +
                                            theme(legend.position = "none")
### In the casual rider group there were a lot rides that lasted more than 1 day, I think these should be removed as casual rider include only
### single-ride passes and full-day passes so it is highly unlikely that casual member will have ride beyond 24 hrs (1440 minutes). Thus, I elected to remove
### rides that were more than 1440 minutes.
divvy_data %>% 
  filter(ride_length_min > 1440) %>% 
  group_by(member_casual) %>% 
  summarize(n = n(),
            percent_total = (n()/dim(divvy_data)[1]) * 100) # less than 0.01% of data is over 1440 minutes

### remove data with ride length > 1440 and plot the distribution again
divvy_data <- divvy_data %>% 
                filter(ride_length_min <= 1440)
sample_data <- sample_n(divvy_data, dim(divvy_data)[1] * 0.2)
distribution_plot <- sample_data %>% ggplot(aes(x = member_casual, y = ride_length_min, color = member_casual)) +
                                            geom_violin() +
                                            coord_flip() +
                                            geom_boxplot(width = .2) +
                                            geom_point(position = position_jitter(seed = 1, width = 0.2)) +
                                            theme(legend.position = "none")


# Descriptive statistics of ride length
summary(divvy_data$ride_length_min)

## Descriptive statistics of ride length via membership
member_summary <- divvy_data %>% 
                   group_by(member_casual) %>% 
                   summarize(mean = mean(ride_length_min),
                             sd = sd(ride_length_min),
                             median = median(ride_length_min),
                             maximum = max(ride_length_min),
                             minimum = min (ride_length_min),
                             n = n())
## Bar plot displaying average ride length and number of users across membership type
member_summary %>% ggplot(aes(x = member_casual, y = mean, fill = member_casual)) +
                         geom_bar(stat = "identity", width = 0.7) +
                         ggtitle("Averaged ride length in each membership type") +
                         ylab("Averaged ride length (minutes)") +
                         xlab("Membership") +
                         theme_minimal() +
                         geom_text(aes(label = paste0("number of trips: ", round(n, 2))), vjust = 1.6, size = 3.5, color = 'black') +
                         geom_text(aes(label = paste0("mean duration: ", round(mean, 2))), vjust = -0.3, size = 3.5, color = 'black') +
                         theme(legend.position = 'none')


# Descriptive statistics of ride length via membership and day of the week
member_days_summary <- divvy_data %>% 
                            group_by(member_casual, day_of_week) %>% 
                            summarize(mean = mean(ride_length_min),
                                      sd = sd(ride_length_min),
                                      median = median(ride_length_min),
                                      maximum = max(ride_length_min),
                                      minimum = min (ride_length_min),
                                      n = n()) %>% 
                            arrange(member_casual, day_of_week)

## Bar plot displaying average ride length and number of riders across days of the week partition by membership type
member_days_summary %>% ggplot(aes(x = day_of_week, y = mean, fill = member_casual)) +
                               geom_bar(stat = "identity", position = 'dodge') +
                               labs(title = "Averaged ride length on each day for each membership type",
                                    fill = "Membership type", 
                                    caption = "number of trips are displayed within plot") +
                               ylab("Averaged ride length (minutes)") +
                               xlab("Days of the week") +
                               theme_minimal() +
                               geom_text(aes(label = n), vjust = 1.6, size = 3.5, color = 'black', position = position_dodge(.9))


###############################################################################################################


