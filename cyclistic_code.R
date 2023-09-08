library('xml2')
library(tidyverse)
library(fs)
library(dplyr)
library(stringr)
library(data.table)
library("lubridate")
library(ggplot2)

trips_01_2022 <- fread("cyclistic_2022/202201-divvy-tripdata.csv")
trips_02_2022 <- fread("cyclistic_2022/202202-divvy-tripdata.csv")
trips_03_2022 <- fread("cyclistic_2022/202203-divvy-tripdata.csv")
trips_04_2022 <- fread("cyclistic_2022/202204-divvy-tripdata.csv")
trips_05_2022 <- fread("cyclistic_2022/202205-divvy-tripdata.csv")
trips_06_2022 <- fread("cyclistic_2022/202206-divvy-tripdata.csv")
trips_07_2022 <- fread("cyclistic_2022/202207-divvy-tripdata.csv")
trips_08_2022 <- fread("cyclistic_2022/202208-divvy-tripdata.csv")
trips_09_2022 <- fread("cyclistic_2022/202209-divvy-tripdata.csv")
trips_10_2022 <- fread("cyclistic_2022/202210-divvy-tripdata.csv")
trips_11_2022 <- fread("cyclistic_2022/202211-divvy-tripdata.csv")
trips_12_2022 <- fread("cyclistic_2022/202212-divvy-tripdata.csv")

cyclistic_trips_2022 <- rbind(trips_01_2022,
                              trips_02_2022,
                              trips_03_2022,
                              trips_04_2022,
                              trips_05_2022,
                              trips_06_2022,
                              trips_07_2022,
                              trips_08_2022,
                              trips_09_2022,
                              trips_10_2022,
                              trips_11_2022,
                              trips_12_2022)

glimpse(cyclistic_trips_2022)


# Eliminate any potential duplicates in ride_id. This is the primary unique ID field.
cyclistic_trips_2022 %>%
  distinct(ride_id, .keep_all = TRUE)

# Look for NA values
sapply(cyclistic_trips_2022, function(x) sum(is.na(x)))
# 5858 entries have an NA value for end_lat and end_lng. All end station data is blank.
na_rows <- subset(cyclistic_trips_2022, is.na(cyclistic_trips_2022$end_lat))
# Remove entries with NA values
cyclistic_trips_2022_clean <- na.omit(cyclistic_trips_2022)

glimpse(cyclistic_trips_2022_clean)

# Check "casual" and member" are the only two options in the member_casual column
unique(cyclistic_trips_2022_clean$member_casual)

# Check "electric_bike", "classic_bike", and "docked_bike" are the only options in the rideable_type column
unique(cyclistic_trips_2022_clean$rideable_type)

# Add a ride length column
cyclistic_trips_2022_clean$ride_length <- difftime(cyclistic_trips_2022_clean$ended_at, cyclistic_trips_2022_clean$started_at)
range(cyclistic_trips_2022_clean$ride_length)
# Convert ride_length into hours, minutes, seconds
cyclistic_trips_2022_clean = cyclistic_trips_2022_clean %>%
  mutate(hours = hour(seconds_to_period(cyclistic_trips_2022_clean$ride_length)),
         minutes = minute(seconds_to_period(cyclistic_trips_2022_clean$ride_length)),)
  
#Add date
cyclistic_trips_2022_clean$date <- as.Date(cyclistic_trips_2022_clean$started_at)
# Add trip_month column
cyclistic_trips_2022_clean$month <- format(as.Date(cyclistic_trips_2022_clean$date), "%m")
#Add day_of_week column
cyclistic_trips_2022_clean$day_of_week <- format(as.Date(cyclistic_trips_2022_clean$date), "%A")

# Remove entries with a negative trip length and docked bikes. Docked bikes have been taken out of circulation so we remove them from our analysis.
trip_data = cyclistic_trips_2022_clean[!(cyclistic_trips_2022_clean$rideable_type == "docked_bike" | cyclistic_trips_2022_clean$ride_length<0),]

glimpse(trip_data)

#remove any rows with empty strings for the start_station_name, end station, or trip id
trip_data = trip_data[!trip_data$ride_id=="",]
trip_data = trip_data[!trip_data$start_station_name=="",]
trip_data = trip_data[!trip_data$end_station_name=="",]

#Analyze

trip_data$day_of_week <- factor(trip_data$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday"))

#Average ride length vs. day of the week
trip_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_duration = mean(minutes), .groups = "drop") %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
    geom_col(width = 0.4, position = position_dodge(width = 0.5)) +
  labs(title = "Average ride length vs. day of the week", x = 'Day of Week', y = 'Average Trip Duration')
#Total Ride Count vs. day of the week
trip_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = day_of_week, y = format(number_of_rides, scientific = FALSE), fill = member_casual)) +
  geom_col(width = 0.4, position = position_dodge(width = 0.5)) +
  labs(title = "Total Ride Count vs. day of the week", x = 'Day of Week', y = 'Total Rides')

# AVG by month
trip_data %>%
  group_by(member_casual, month) %>%
  summarise(average_duration = mean(minutes), .groups = "drop") %>%
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(width = 0.4, position = position_dodge(width = 0.5)) +
  labs(title = "Average ride length vs. Month", x = 'Month', y = 'Average Trip Duration')
# Total by month
trip_data %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = month, y = format(number_of_rides, scientific = FALSE), fill = member_casual)) +
  geom_col(width = 0.4, position = position_dodge(width = 0.5)) +
  labs(title = "Total Ride Count vs. Month", x = 'Month', y = 'Total Rides')



#Find top 5 start stations by ride count for members and casual customers
station_rides_count <- trip_data %>% group_by(start_station_name, member_casual) %>%
  summarise(total_count=n(), .groups='drop') %>%
  as.data.frame()
#station_rides_count

top_stations <- station_rides_count %>%                                      # Top N highest values by group
  arrange(desc(total_count)) %>% 
  group_by(member_casual) %>%
  slice(1:5)
top_stations   
