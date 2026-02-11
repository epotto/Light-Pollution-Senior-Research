# This script will be used to do data analysis for the SQM at CMU specifically.

# Ethan Otto


 
library(tidyverse)
library(lubridate)
library(dplyr)
library(lubridate)
library(hms)

data <- read.csv("SQM_Readings.csv")

trial2_data <- data[,1:12]

# something to save

trial2_dat_structured <- trial2_data |>
  group_by(Location,Date,Category)


#git push --set-upstream origin master

trial2_dat_dates <- trial2_dat_structured |>
  ungroup() |>
  group_by(Date) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |>
  reframe(Date)

# Better try for sqm means

trail2_sqm1_mean <- aggregate(SQM1~ Time + Date + Location + Category + 
                                Weather + Humidity + Cloud_Cover + 
                                Moon_Brightness + Moon_Cycle,trial2_dat_structured, 
                              FUN = mean) |>
  ungroup() |>
  reframe(Date,Time,Category,Location,SQM1,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)


trail2_sqm2_mean <- aggregate(SQM2~ Time + Date + Location + Category + 
                                Weather + Humidity + Cloud_Cover + 
                                Moon_Brightness + Moon_Cycle,trial2_dat_structured, 
                              FUN = mean) |>
  ungroup() |>
  reframe(Date,Time,Category,Location,SQM2,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)


trail2_pre_sqm_means <- right_join(trail2_sqm1_mean,trail2_sqm2_mean)


# Now to join the mean of each SQM device.

trial2_only_sqm_means <- trail2_pre_sqm_means |>
  select(SQM1,SQM2)

trial2_sqm_mean <- trail2_pre_sqm_means|>
  mutate(SQM = rowMeans(trial2_only_sqm_means)) |>
  group_by(Category,Date) |>
  reframe(Date,Time,Category,Location,
          SQM1,SQM2,SQM,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)
#print(trial2_sqm_mean)

mean_in_data <- trial2_sqm_mean |>
  ungroup() |>
  group_by(Date,Time,Location) |>
  reframe(Date,Time,Category,Location,SQM,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)


#
# Moon cycle stuff:
#

# 1. New Moon -> 2. Waxing Crescent -> 3. First Quarter -> 4. Waxing Gibbous -> 5. Full Moon -> 6. Waning Gibbous -> 7. Third Quarter (Last Quarter) -> 8. Waning Crescent -> ...

unique(mean_in_data$Moon_Cycle)


moon_with_mean <- mean_in_data |>
  mutate(Phase_Num = moon_func(mean_in_data))


# Working attempt! : 

moon_func <- function(data) {
  #dataset <- data.frame(1:length(data$Moon_Cycle))
  dataset <- list()
  for(i in 1:length(data$Moon_Cycle)){
    if(data$Moon_Cycle[i] == "New_Moon"){
      dataset[[i]] <- 1
    } else if(data$Moon_Cycle[i] == "Waxing_Crescent"){
      dataset[[i]] <- 2
    }else if(data$Moon_Cycle[i] == "First_Quarter"){
      dataset[[i]] <- 3
    }else if(data$Moon_Cycle[i] == "Waxing_Gibbous"){
      dataset[[i]] <- 4
    }else if(data$Moon_Cycle[i] == "Full_Moon"){
      dataset[[i]] <- 5
    }else if(data$Moon_Cycle[i] == "Waning_Gibbous"){
      dataset[[i]] <- 6
    }else if(data$Moon_Cycle[i] == "Last_Quarter" | data$Moon_Cycle[i] == "Third Quarter"){
      dataset[[i]] <- 7
    }else if(data$Moon_Cycle[i] == "Waning_Crescent"){
      dataset[[i]] <- 8
    }
  }
  return(dataset)
}

  

#CMU
cmu_sqm_data <- moon_with_mean |>
  filter(Category == "CMU") |>
  reframe(Date,Time,Category,Location,SQM,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle,Phase_Num)



#Rimrock
rim_sqm_data <- moon_with_mean |>
  filter(Category == "Rimrock") |>
  reframe(Date,Time,Category,Location,SQM,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle,Phase_Num)


#Apartment
apt_sqm_data <- moon_with_mean |>
  filter(Category == "Rock_Slide") |>
  ungroup() |>
  group_by(Date) |>
  reframe(Date,Time,Category,Location,SQM,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle,Phase_Num)



ggplot(data = apt_sqm_data,
       aes(x = Date,
           y = SQM,
           color = Weather)) +
  geom_point(size = 5)

ggplot(data = mean_in_data,
       aes(x = Date,
           y = SQM,
           color = Weather)) +
  geom_point(size = 5)


ggplot(data = apt_sqm_data,
       aes(x = Date,
           y = SQM,
           color = Cloud_Cover)) +
  geom_point(size = 5)


ggplot(data = mean_in_data,
       aes(x = Date,
           y = SQM,
           color = Cloud_Cover)) +
  geom_point(size = 5)

ggplot(data = mean_in_data,
       aes(x = Moon_Cycle,
           y = SQM,
           color = Weather)) +
  labs(title = "Moon Cycle all data") +
  geom_point(size = 5)


ggplot(data = apt_sqm_data,
       aes(x = Phase_Num,
           y = SQM,
           color = Weather)) +
  labs(title = "Moon Cycle all data") +
  geom_point(size = 5)



#Statistical tests below:

######
# Statistical Tests:



# Plot our data with and without groups

# Global distribution

# mean_in_data is our blanket dataset for now.

# names() is how you see the unique column names.
names(mean_in_data)

ggplot(mean_in_data,
       aes(x = 0,
           y = SQM)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.1))

# Boxplot summarises a bunch of numbers into a boxplot

# Jitter adds horizontal noise so we can see every point


# Plot groups












# Use NOAA for Weather

#####
# This is an attempt to get code working/data working from NOAA

noaa_2025_raw <- read.delim("NOAA_2025.psv", sep = "|", header = TRUE)

noaa_2026_raw <- read.delim("NOAA_2026.psv", sep = "|", header = TRUE)

# Sky cover is in terms of 1/8th's of the sky

noaa_2025_filt <- noaa_2025_raw |>
  ungroup() |>
  reframe(STATION, Station_name, DATE, precipitation_3_hour, precipitation, relative_humidity,
          visibility, sky_cover_1, sky_cover_1_Measurement_Code, sky_cover_2_Measurement_Code, sky_cover_2, sky_cover_3, sky_cover_3_Measurement_Code)

noaa_2026_filt <- noaa_2026_raw |>
  ungroup() |>
  reframe(STATION, Station_name, DATE, precipitation_3_hour, precipitation, relative_humidity,
          visibility, sky_cover_1, sky_cover_1_Measurement_Code, sky_cover_2_Measurement_Code, sky_cover_2, sky_cover_3, sky_cover_3_Measurement_Code)


# I'm not sure why it works better, but using parse_date_time() worked for getting R to neglect the "T" within the raw date format in the psv file.          #Date = parse_date_time(DATE, orders = "ymd"),Time = parse_date_time(Date, orders = "HMS")) 

# The best way to do it is using as_date() and as_hms()

noaa_2025_date_filt <- noaa_2025_filt |>
  ungroup() |>
  mutate(TIME = ymd_hms(DATE)) |>
  reframe(date = as_date(DATE), time = as_hms(TIME),
          precipitation_3_hour, precipitation, relative_humidity,
          visibility, sky_cover_1, sky_cover_1_Measurement_Code, sky_cover_2_Measurement_Code, sky_cover_2, sky_cover_3, sky_cover_3_Measurement_Code)


noaa_2026_date_filt <- noaa_2026_filt |>
  ungroup() |>
  mutate(TIME = ymd_hms(DATE)) |>
  reframe(date = as_date(DATE), time = as_hms(TIME),
          precipitation_3_hour, precipitation, relative_humidity,
          visibility, sky_cover_1, sky_cover_1_Measurement_Code, sky_cover_2_Measurement_Code, sky_cover_2, sky_cover_3, sky_cover_3_Measurement_Code)


noaa_2025_date_pre_no_test <- noaa_2025_date_filt |>
  filter(date == "2025-08-26")

noaa_2025_date_no_test <- noaa_2025_date_filt |>
  filter(date == "2025-08-27")

unique(noaa_2025_raw)

climate_test_data <- read.csv("GJ_Climate_Data.csv")

climate_test_filt <- climate_test_data |>
  ungroup() |>
  mutate(TIME = ymd_hms(DATE)) |>
  mutate(date = as_date(DATE)) |>
  mutate(time = as_hms(TIME)) |>
  select(-c(DATE))

#
# Now, we can filter using dates and time!!
#


# Usefull for switching time zones:

time <- ymd_hms("2010-12-13 15:30:30")
time
#> [1] "2010-12-13 15:30:30 UTC"

# Changes printing
with_tz(time, "America/Chicago")
#> [1] "2010-12-13 09:30:30 CST"

# Changes time
force_tz(time, "America/Chicago")
#> [1] "2010-12-13 15:30:30 CST"


#
# It appears that this is as simple as it will get, at least without more advanced code that I can do at the moment. Can't filter by date because of format, and timing will be difficult to discern.

#Unfortunately, this means we must take weather data manually.
#








# First major attempt with edits that broke code. 
# Encorporates advanced techniques, but incorrectly is executed.
# Could be useful to look at later.








#####

# This is entirely broken. I'm going to make code above it and encorporate the code that worked before, but didn't include time.


dat_structured <- data |>
  group_by(Category, Date, Location)


dat_dates <- dat_structured |>
  ungroup() |>
  group_by(Date, Time) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |>
  reframe(Date, Time)

time_dat <- dat_structured |>
  ungroup() |>
  group_by(Date, Time) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |>
  ungroup() |>
  reframe(Time) |>
  summarise(Time = dmy_hms(Time))

test_dates<- dat_structured |>
  ungroup() |>
  group_by(Date,Time) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |>
  mutate(Date_Time = dmy_hm(paste(Date,Time)))

test_stuff <- dat_structured |>
  ungroup() |>
  group_by(Date,Time) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |>
  mutate(Date_Time = paste(Date,Time)) |>
  reframe(Date_Time = dmy_hm(Date_Time))

time_dat_test2 <- dat_structured |>
  ungroup() |>
  group_by(Date) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "")


sqm1_mean <- aggregate(SQM1~Location,data, FUN = mean) |>
  ungroup() |>
  reframe(Location, SQM1)
sqm2_mean <- aggregate(SQM2~Location,data, FUN = mean) |>
  ungroup() |>
  reframe(Location,SQM2)


pre_sqm_means <- right_join(sqm1_mean,sqm2_mean)

test1_sqm_join <- left_join(pre_sqm_means,test_dates)

test1_sqm_join2 <- left_join(test_sqm_join,time)


test1_sqm_join <- left_join(pre_sqm_means,test_dates)

test1_sqm_join2 <- left_join(test_sqm_join,time)

# Working attempt

structured_sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  reframe(Category,Date,Location,SQM1,SQM2) |>
  select(SQM1,SQM2)

final_sqm_mean <- right_join(pre_sqm_means,dat_dates) |>
  mutate(SQM = rowMeans(structured_sqm_means)) |>
  group_by(Category,Date) |>
  reframe(Category,Date,Location,SQM1,SQM2,SQM)
print(final_sqm_mean)

#print(structured_sqm_means)

final_sqm_mean <- right_join(pre_sqm_means,dat_dates) |>
  mutate(SQM = rowMeans(structured_sqm_means)) |>
  #mutate(Time = unique(data$Time)) |>
  group_by(Category,Date,Time) |>
  reframe(Category,Date,Time,Location,SQM1,SQM2,SQM)
print(final_sqm_mean)


# Right Join technique

sqm_means <- right_join(pre_sqm_means,dat_dates)

total_sqm_mean <- sqm_means |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2) |>
  select(SQM1,SQM2) |>
  mutate(SQM = rowMeans(sqm_means)) |>
  right_join(sqm_means,total_sqm_mean) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(sqm_means)

#CMU
cmu_sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  filter(Category == "CMU") |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(cmu_sqm_means)


#Rimrock
rim_sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  filter(Category == "Rimrock") |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(rim_sqm_means)

#Apartment
apt_sqm_means <- final_sqm_mean |>
  filter(Category == "Rock_Slide") |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2,SQM)
print(apt_sqm_means)









# This script will be used to do data analysis for the SQM at CMU specifically.

# Ethan Otto

library(tidyverse)
library(dplyr)

data <- read.csv("SQM_Readings_For_R.csv")

dat_structured <- data |>
  group_by(Category, Date, Location)

dat_dates <- dat_structured |>
  summarise(Date = unique(Date)) |>
  filter(Date != "")


sqm1_mean <- aggregate(SQM1~Location,data, FUN = mean)
sqm2_mean <- aggregate(SQM2~Location,data, FUN = mean)

pre_sqm_means <- right_join(sqm1_mean,sqm2_mean)


# 2nd attempt

sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  group_by(Category) |>
  select(SQM1,SQM2) |>
  mutate(SQM = rowMeans(sqm_means)) |>
  reframe(Category,Date,Location,SQM)
print(sqm_means)

 
#Working attempt

structured_sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  reframe(Category,Date,Location,SQM1,SQM2) |>
  select(SQM1,SQM2)
#print(structured_sqm_means)

final_sqm_mean <- right_join(pre_sqm_means,dat_dates) |>
  mutate(SQM = rowMeans(structured_sqm_means)) |>
  group_by(Category,Date) |>
  reframe(Category,Date,Location,SQM1,SQM2,SQM)
print(final_sqm_mean)


# 1st attempt

sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  group_by(Category) |>
  mutate(SQM = rowMeans(structured_sqm_means)) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(sqm_means)

# 2nd attempt

sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  mutate(SQM = rowMeans(structured_sqm_means)) |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2,SQM)
print(sqm_means)

#Backup in case things go wrong

sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2) |>
  select(SQM1,SQM2) |>
  mutate(SQM = rowMeans(sqm_means))
print(sqm_means)

# Right Join technique

sqm_means <- right_join(pre_sqm_means,dat_dates)

total_sqm_mean <- sqm_means |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2) |>
  select(SQM1,SQM2) |>
  mutate(SQM = rowMeans(sqm_means)) |>
  right_join(sqm_means,total_sqm_mean) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(sqm_means)

#CMU
cmu_sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  filter(Category == "CMU") |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(cmu_sqm_means)


#Rimrock
rim_sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  filter(Category == "Rimrock") |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(rim_sqm_means)

#Apartment
apt_sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  filter(Category == "Rock_Slide") |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(apt_sqm_means)









# This script will be used to do data analysis for the SQM at CMU specifically.

# Ethan Otto



library(tidyverse)
library(lubridate)
library(dplyr)
library(lubridate)
library(hms)

data <- read.csv("SQM_Readings.csv")

trial2_data <- data[,1:12]



trial2_dat_structured <- trial2_data |>
  group_by(Location,Date,Category)


trial2_dat_dates <- trial2_dat_structured |>
  ungroup() |>
  group_by(Date) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |>
  reframe(Date)

# Better try for sqm means

trail2_sqm1_mean <- aggregate(SQM1~ Time + Date + Location + Category + 
                                Weather + Humidity + Cloud_Cover + 
                                Moon_Brightness + Moon_Cycle,trial2_dat_structured, 
                              FUN = mean) |>
  ungroup() |>
  reframe(Date,Time,Category,Location,SQM1,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)


trail2_sqm2_mean <- aggregate(SQM2~ Time + Date + Location + Category + 
                                Weather + Humidity + Cloud_Cover + 
                                Moon_Brightness + Moon_Cycle,trial2_dat_structured, 
                              FUN = mean) |>
  ungroup() |>
  reframe(Date,Time,Category,Location,SQM2,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)


trail2_pre_sqm_means <- right_join(trail2_sqm1_mean,trail2_sqm2_mean)


# Now to join the mean of each SQM device.

trial2_only_sqm_means <- trail2_pre_sqm_means |>
  select(SQM1,SQM2)

trial2_sqm_mean <- trail2_pre_sqm_means|>
  mutate(SQM = rowMeans(trial2_only_sqm_means)) |>
  group_by(Category,Date) |>
  reframe(Date,Time,Category,Location,
          SQM1,SQM2,SQM,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)
#print(trial2_sqm_mean)

mean_in_data <- trial2_sqm_mean |>
  ungroup() |>
  group_by(Date,Time,Location) |>
  reframe(Date,Time,Category,Location,SQM,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)




#CMU
cmu_sqm_data <- mean_in_data |>
  filter(Category == "CMU") |>
  reframe(Date,Time,Category,Location,SQM,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)



#Rimrock
rim_sqm_data <- mean_in_data |>
  filter(Category == "Rimrock") |>
  reframe(Date,Time,Category,Location,SQM,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)


#Apartment
apt_sqm_data <- mean_in_data |>
  filter(Category == "Rock_Slide") |>
  ungroup() |>
  group_by(Date) |>
  reframe(Date,Time,Category,Location,SQM,
          Weather,Cloud_Cover,Humidity,
          Moon_Brightness,Moon_Cycle)


print(apt_sqm_data,n=24)


ggplot(data = apt_sqm_data,
       aes(x = Date,
           y = SQM,
           color = Weather)) +
  geom_point(size = 5)


ggplot(data = apt_sqm_data,
       aes(x = Date,
           y = SQM,
           color = Cloud_Cover)) +
  geom_point(size = 5)


# Use NOAA for Weather

#####
# This is an attempt to get code working/data working from NOAA

noaa_2025_raw <- read.delim("NOAA_2025.psv", sep = "|", header = TRUE)

noaa_2026_raw <- read.delim("NOAA_2026.psv", sep = "|", header = TRUE)

# Sky cover is in terms of 1/8th's of the sky

noaa_2025_filt <- noaa_2025_raw |>
  ungroup() |>
  reframe(STATION, Station_name, DATE, precipitation_3_hour, precipitation, relative_humidity,
          visibility, sky_cover_1, sky_cover_1_Measurement_Code, sky_cover_2_Measurement_Code, sky_cover_2, sky_cover_3, sky_cover_3_Measurement_Code)

noaa_2026_filt <- noaa_2026_raw |>
  ungroup() |>
  reframe(STATION, Station_name, DATE, precipitation_3_hour, precipitation, relative_humidity,
          visibility, sky_cover_1, sky_cover_1_Measurement_Code, sky_cover_2_Measurement_Code, sky_cover_2, sky_cover_3, sky_cover_3_Measurement_Code)


# I'm not sure why it works better, but using parse_date_time() worked for getting R to neglect the "T" within the raw date format in the psv file.          #Date = parse_date_time(DATE, orders = "ymd"),Time = parse_date_time(Date, orders = "HMS")) 

# The best way to do it is using as_date() and as_hms()

noaa_2025_date_filt <- noaa_2025_filt |>
  ungroup() |>
  mutate(TIME = ymd_hms(DATE)) |>
  reframe(date = as_date(DATE), time = as_hms(TIME),
          precipitation_3_hour, precipitation, relative_humidity,
          visibility, sky_cover_1, sky_cover_1_Measurement_Code, sky_cover_2_Measurement_Code, sky_cover_2, sky_cover_3, sky_cover_3_Measurement_Code)


noaa_2026_date_filt <- noaa_2026_filt |>
  ungroup() |>
  mutate(TIME = ymd_hms(DATE)) |>
  reframe(date = as_date(DATE), time = as_hms(TIME),
          precipitation_3_hour, precipitation, relative_humidity,
          visibility, sky_cover_1, sky_cover_1_Measurement_Code, sky_cover_2_Measurement_Code, sky_cover_2, sky_cover_3, sky_cover_3_Measurement_Code)



#
# Now, we can filter using dates and time!!
#


# Usefull for switching time zones:

time <- ymd_hms("2010-12-13 15:30:30")
time
#> [1] "2010-12-13 15:30:30 UTC"

# Changes printing
with_tz(time, "America/Chicago")
#> [1] "2010-12-13 09:30:30 CST"

# Changes time
force_tz(time, "America/Chicago")
#> [1] "2010-12-13 15:30:30 CST"


#
# It appears that this is as simple as it will get, at least without more advanced code that I can do at the moment. Can't filter by date because of format, and timing will be difficult to discern.

#Unfortunately, this means we must take weather data manually.
#








# First major attempt with edits that broke code. 
# Encorporates advanced techniques, but incorrectly is executed.
# Could be useful to look at later.








#####

# This is entirely broken. I'm going to make code above it and encorporate the code that worked before, but didn't include time.


dat_structured <- data |>
  group_by(Category, Date, Location)


dat_dates <- dat_structured |>
  ungroup() |>
  group_by(Date, Time) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |>
  reframe(Date, Time)

time_dat <- dat_structured |>
  ungroup() |>
  group_by(Date, Time) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |>
  ungroup() |>
  reframe(Time) |>
  summarise(Time = dmy_hms(Time))

test_dates<- dat_structured |>
  ungroup() |>
  group_by(Date,Time) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |>
  mutate(Date_Time = dmy_hm(paste(Date,Time)))

test_stuff <- dat_structured |>
  ungroup() |>
  group_by(Date,Time) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |>
  mutate(Date_Time = paste(Date,Time)) |>
  reframe(Date_Time = dmy_hm(Date_Time))

time_dat_test2 <- dat_structured |>
  ungroup() |>
  group_by(Date) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "")


sqm1_mean <- aggregate(SQM1~Location,data, FUN = mean) |>
  ungroup() |>
  reframe(Location, SQM1)
sqm2_mean <- aggregate(SQM2~Location,data, FUN = mean) |>
  ungroup() |>
  reframe(Location,SQM2)


pre_sqm_means <- right_join(sqm1_mean,sqm2_mean)

test1_sqm_join <- left_join(pre_sqm_means,test_dates)

test1_sqm_join2 <- left_join(test_sqm_join,time)


test1_sqm_join <- left_join(pre_sqm_means,test_dates)

test1_sqm_join2 <- left_join(test_sqm_join,time)

# Working attempt

structured_sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  reframe(Category,Date,Location,SQM1,SQM2) |>
  select(SQM1,SQM2)

final_sqm_mean <- right_join(pre_sqm_means,dat_dates) |>
  mutate(SQM = rowMeans(structured_sqm_means)) |>
  group_by(Category,Date) |>
  reframe(Category,Date,Location,SQM1,SQM2,SQM)
print(final_sqm_mean)

#print(structured_sqm_means)

final_sqm_mean <- right_join(pre_sqm_means,dat_dates) |>
  mutate(SQM = rowMeans(structured_sqm_means)) |>
  #mutate(Time = unique(data$Time)) |>
  group_by(Category,Date,Time) |>
  reframe(Category,Date,Time,Location,SQM1,SQM2,SQM)
print(final_sqm_mean)


# Right Join technique

sqm_means <- right_join(pre_sqm_means,dat_dates)

total_sqm_mean <- sqm_means |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2) |>
  select(SQM1,SQM2) |>
  mutate(SQM = rowMeans(sqm_means)) |>
  right_join(sqm_means,total_sqm_mean) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(sqm_means)

#CMU
cmu_sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  filter(Category == "CMU") |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(cmu_sqm_means)


#Rimrock
rim_sqm_means <- right_join(pre_sqm_means,dat_dates) |>
  filter(Category == "Rimrock") |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2)
print(rim_sqm_means)

#Apartment
apt_sqm_means <- final_sqm_mean |>
  filter(Category == "Rock_Slide") |>
  group_by(Category) |>
  reframe(Category,Date,Location,SQM1,SQM2,SQM)
print(apt_sqm_means)








