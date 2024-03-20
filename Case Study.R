install.packages("tidyverse")
install.packages("conflicted")

library(tidyverse)
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#Collecting the data

m1_2023 <- read.csv("C:\\csv files\\202301-divvy-tripdata.csv")
m2_2023 <- read.csv("C:\\csv files\\202302-divvy-tripdata.csv")
m3_2023 <- read.csv("C:\\csv files\\202303-divvy-tripdata.csv")
m4_2023 <- read.csv("C:\\csv files\\202304-divvy-tripdata.csv")
m5_2023 <- read.csv("C:\\csv files\\202305-divvy-tripdata.csv")
m6_2023 <- read.csv("C:\\csv files\\202306-divvy-tripdata.csv")
m7_2023 <- read.csv("C:\\csv files\\202307-divvy-tripdata.csv")
m8_2023 <- read.csv("C:\\csv files\\202308-divvy-tripdata.csv")
m9_2023 <- read.csv("C:\\csv files\\202309-divvy-tripdata.csv")
m10_2023 <- read.csv("C:\\csv files\\202310-divvy-tripdata.csv")
m11_2023 <- read.csv("C:\\csv files\\202311-divvy-tripdata.csv")
m12_2023 <- read.csv("C:\\csv files\\202312-divvy-tripdata.csv")


#The date format of m1_2023 seems to be the only one with %d-%m-%Y %H:%M so standardizing it

m1_2023$started_at <- as.POSIXct(m1_2023$started_at, format = "%d-%m-%Y %H:%M") 
m1_2023$started_at <- format(m1_2023$started_at, "%Y-%m-%d %H:%M:%S")

m1_2023$ended_at <- as.POSIXct(m1_2023$ended_at, format = "%d-%m-%Y %H:%M") 
m1_2023$ended_at <- format(m1_2023$ended_at, "%Y-%m-%d %H:%M:%S")

#merging all the data into a single dataframe

library(dplyr)
all_trips <- bind_rows(m1_2023, m2_2023, m3_2023, m4_2023, m5_2023, m6_2023, m7_2023, m8_2023, m9_2023, m10_2023, m11_2023, m12_2023)

#deleting all the individual monthly dataframe

rm(m1_2023, m2_2023, m3_2023, m4_2023, m5_2023, m6_2023, m7_2023, m8_2023, m9_2023, m10_2023, m11_2023, m12_2023)

#Cleaning and adding data to prepare for analysis

colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# Begin by seeing how many observations fall under each usertype and rideable type
table(all_trips$member_casual)
table(all_trips$rideable_type)

ride_type_summary <- all_trips_v2 %>%
  group_by(rideable_type, member_casual) %>%
  summarize(count = n())

print(ride_type_summary)

# Convert dates with specified format

all_trips$date <- as.Date(all_trips$started_at, format = "%Y-%m-%d")

View(all_trips)

all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")

all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#Cleaning the duplicates

all_trips_v2 <- distinct(all_trips)
rm(all_trips_v2) #Deleting as there are no duplicates


#Adding ride_length to calculate the duration of the ride

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

#Making sure that the ride_length is in numerics
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#Checking if there are any invalid ride_length
all_trips_v2 <- all_trips[is.na(all_trips$ride_length) | !(all_trips$ride_length <= 0), ]

#Making sure there was no error in calculating ride_length

all_trips_v3 <- all_trips[(all_trips$ride_length<0),] # 272 objects due to the started_at being the time after ended_at
rm(all_trips_v3)


#Descriptive analysis


mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

#min is 0 is checking for any ride_length that is 0 secs

all_trips_v3 <- all_trips_v2[(all_trips_v2$ride_length <= 0), ] # 4800 initially, now none

# all_trips_v3 <- all_trips[(all_trips$start_station_id != all_trips$end_station_id), ] #isn't producing the right result
#Corrected the previous from all_trips$ride_length < 0 to <=

summary(all_trips_v2$ride_length)

# Compare members and casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


# See the average ride time by each day for members vs casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


# Fixing the order of the days in days of the week.(Doesn't change values in the dataframe but affects the day_of_week variable when it comes to ordering and plotting.)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


#Pipes, Grouping and Sorting in R
# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% #calculates the number of rides and average duration
  arrange(member_casual, weekday)	# sorts

#In the above code the original data frame didn't change as the assignment operator wasn't used all_trips_v2 <- all_trips_v2 %>% ...


#Data viz, finally!!

options(scipen = 999) #Applied as the number of rides were being shown in scientific notation

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Day of Week", x = "Day of Week", y = "Number of Rides", fill = "User Type")



#Visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title ="Average duration of the Rides by Day of Week", x = "Day of Week", y = "Avg duration of the Rides", fill = "User Type")


#Exporting the csv

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

colnames(counts) <- c("Member Type", "Day of the Week", "Average Ride Length")

# Write the data frame to the CSV file
write.csv(counts, file = 'avg_ride_length.csv', row.names = FALSE)


# Ride type summary in stacked bar chart as docked_bike is all casual

library(stringr)

# Convert rideable_type to title case
ride_type_summary$rideable_type <- str_replace_all(ride_type_summary$rideable_type, "_", " ")
ride_type_summary$rideable_type <- str_to_title(ride_type_summary$rideable_type)

ggplot(ride_type_summary, aes(x = rideable_type, y = count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labs(title = "Rides Taken by Different Member Types and Rideable Types",
       x = "Rideable Type",
       y = "Count",
       fill = "Member Type") +
  theme_minimal()
