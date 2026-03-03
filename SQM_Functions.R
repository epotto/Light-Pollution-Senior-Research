# This script will be used to do data analysis for the SQM at CMU specifically.

# Ethan Otto


library(tidyverse)
library(lubridate)
library(dplyr)
library(lubridate)
library(hms)

data <- read.csv("SQM_Readings_for_R.csv")

names(data)


#Not usefull yet...
variable_processer <- function(data){
  i <- readline(prompt = "Which variable set? (Remember to use 'data' for raw_var and 'sqm_mean_all_data' for var and moon_var)
                input 1 for raw, 2 for all usefull var, or 3 for moon_var")
  if(i ==1){
    dat_filt <- data[,1:17]
    return(names(dat_filt))
  }else if(i ==2){
    return(names(data))
  }else if(i ==3){
    moon_filt <- data |>
      select(Location, Date, Time, Category, SQM, Moon_Cycle, Phase., 
             Moon_Brightness, Moon_Magnitude, Moon_Altitude)
    return(names(moon_filt))
  }
}

raw_var <- variable_processer(data)
moon_var <- variable_processer(sqm_mean_all_data)

# Basic processer works to give mean of all right now.
# I also added a function to find standard deviation.

basic_processer <- function(data){
  dat_filt <- data[,1:17] |>
    group_by(Location,Date,Time, Category)
  date_time <- dat_filt |>
    ungroup() |>
    group_by(Date, Time) |>
    summarise(Date = unique(Date)) |>
    filter(Date != "") |> 
    mutate(Date_Time = mdy_hms(paste(Date,Time))) 
  
  sqm1_mean <- aggregate(SQM1~ Time + Date + Location + Category + 
                           Weather + Relative_Humidity + Cloud_Cover + 
                           Moon_Brightness + Moon_Cycle + Moon_Magnitude + Moon_Altitude,dat_filt, 
                         FUN = mean) |>
    ungroup() |>
    reframe(Date,Time,Category,Location,SQM1,
            Weather,Cloud_Cover,Relative_Humidity,
            Moon_Brightness,Moon_Cycle, Moon_Magnitude, Moon_Altitude)
  
  sqm2_mean <- aggregate(SQM2~ Time + Date + Location + Category + 
                           Weather + Relative_Humidity + Cloud_Cover + 
                           Moon_Brightness + Moon_Cycle + Moon_Magnitude + Moon_Altitude,dat_filt, 
                         FUN = mean) |>
    ungroup() |>
    reframe(Date,Time,Category,Location,SQM2,
            Weather,Cloud_Cover,Relative_Humidity,
            Moon_Brightness,Moon_Cycle, Moon_Magnitude, Moon_Altitude)
  variance <- aggregate(SQM1~ Time + Date + Location + Category + 
                     Weather + Relative_Humidity + Cloud_Cover + 
                     Moon_Brightness + Moon_Cycle + Moon_Magnitude + Moon_Altitude,dat_filt, 
                   FUN = sd) |>
    reframe(variance = SQM1 / sqrt(6)) |>
    select(variance)
  variance2 <- aggregate(SQM2~ Time + Date + Location + Category + 
                     Weather + Relative_Humidity + Cloud_Cover + 
                     Moon_Brightness + Moon_Cycle + Moon_Magnitude + Moon_Altitude,dat_filt, 
                   FUN = sd) |>
    reframe(variance2 = SQM2 / sqrt(6)) |>
    select(variance2)
    
  pre_sqm_means <- right_join(sqm1_mean,sqm2_mean)
  total_variance <- sqrt(variance^2 + variance2^2 + 2*(0.1)^2)/2
  sqm_means_data <- mutate(total_variance, pre_sqm_means)
  
  # Now to join the mean of each SQM device.
  
  only_sqm_means <- sqm_means_data |>
    select(SQM1,SQM2)
  
  sqm_mean <- right_join(sqm_means_data, date_time)|>
    mutate(SQM = rowMeans(only_sqm_means)) |>
  group_by(Category,Date_Time) |>
    reframe(Date_Time,Category,Location,SQM, variance,
            Weather,Cloud_Cover,Relative_Humidity,
            Moon_Brightness,Moon_Cycle, Moon_Magnitude, Moon_Altitude)
}

sqm_mean_all_data <- basic_processer(data)





#variables <- variable_processer(data)



# Now I'll filter based on category...

# What I'm doing is giving it 2 inputs; data, and a number (1, 2, or 3).
# This allows us to make master functions more easily.

location_distinguisher <- function(data, i){
  cmu_sqm_data <- data |>
    filter(Category == "CMU") 
  
  rim_sqm_data <- data |>
    filter(Category == "Rimrock") 
  
  apt_sqm_data <- data |>
    filter(Category == "Rock_Slide") 
  
  if(i == 1){
    return(cmu_sqm_data)
  } else if(i ==2){
    return(rim_sqm_data)
  } else if(i ==3){
    return(apt_sqm_data)
  }
}

cmu_sqm_data <- location_distinguisher(sqm_mean_all_data,1)
rim_sqm_data <- location_distinguisher(sqm_mean_all_data,2)
apt_sqm_data <- location_distinguisher(sqm_mean_all_data,3)


# This will filter out data that isn't useful for conclusions on the effect of the moon.

moon_above_or_below <- function(data){
  dataset <- list()
  for(i in 1:length(data$Moon_Altitude)){
    if(data$Moon_Altitude[i] > 0){
      dataset[i] <- TRUE
    } else{
      dataset[i] <- FALSE
    }
  }
  return(dataset)
}

moon_filter <- function(data, i){
  if(i == 1){
    cmu_moon_data <- data |>
      filter(Category == "CMU") 
    cmu_moon_data <- cmu_moon_data |>
      mutate(above = moon_above_or_below(cmu_moon_data))
    return(cmu_moon_data)
  } else if(i ==2){
    rim_moon_data <- data |>
      filter(Category == "Rimrock")
    rim_moon_data <- rim_moon_data |>
      mutate(above = moon_above_or_below(rim_moon_data))
    return(rim_moon_data)
  } else if(i ==3){
    apt_moon_data <- data |>
      filter(Category == "Rock_Slide")
    apt_moon_data <- apt_moon_data |>
      mutate(above = moon_above_or_below(apt_moon_data))
    return(apt_moon_data)
  } else if(i ==4){
    all_moon_data <- data |>
      mutate(above = moon_above_or_below(data))
    return(all_moon_data)
  }
}

cmu_moon_data <- moon_filter(sqm_mean_all_data,1)
rim_moon_data <- moon_filter(sqm_mean_all_data,2)
apt_moon_data <- moon_filter(sqm_mean_all_data,3)
all_moon_data <- moon_filter(sqm_mean_all_data,4)

#
# Moon cycle stuff:
#

# 1. New Moon -> 2. Waxing Crescent -> 3. First Quarter -> 4. Waxing Gibbous -> 5. Full Moon -> 6. Waning Gibbous -> 7. Third Quarter (Last Quarter) -> 8. Waning Crescent -> ...

unique(sqm_mean_all_data$Moon_Cycle)


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

moon_with_mean <- mean_in_data |>
  mutate(Phase_Num = moon_func(mean_in_data))




# Variables:

names(all_moon_data)

# Category, Date_Time, Location, SQM, Variance, Weather, Cloud_Cover, Relative_Humidity, Moon_Brightness, Moon_Cycle, Moon_Magnitude, Moon_Altitude, above_below



# Now I'm going to start graphing with my new error bars!!


# Looking at moon magnitude here:

# -2.5 = New Moon magnitude, -12.7 = Full Moon magnitude, -10 to -11 for Quarter Moon (~ 21% as bright as full moon), 

# Let's look at all data, and include when moon is below horizon by color coding!

ggplot(data = all_moon_data,
       aes(x = Moon_Magnitude,
           y = SQM,
           color = as.logical(above))) +
  scale_x_reverse() +
  ylim(10, 17) +
  labs(title = "SQM vs Moon Brightness (Astronomical Scale)") +
  annotate("segment", x = -12.7, xend = -12.7, y = 10, yend = 10.7, 
           color = "black", linewidth = 1) +
  annotate("text", x = -12.7, y = 11, label = "Full Moon", 
           color ="black", angle = 90, hjust = 0, size = 3.5) +
  annotate("segment", x = -10, xend = -10, y = 10, yend = 10.7, 
           color = "black", linewidth = 1) +
  annotate("text", x = -10, y = 11, label = "First Quarter", 
           color ="black", angle = 90, hjust = 0, size = 3.5) +
  #  annotate("segment", x = -2.5, xend = -2.5, y = 10, yend = 11, 
  #           color = "black", linewidth = 1) +
  #  annotate("text", x = -2.5, y = 11.25, label = "New Moon", 
  #           color ="black", angle = 90, hjust = 0, size = 3.5) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), width = 0.075)


ggplot(data = apt_moon_data,
       aes(x = Moon_Magnitude,
           y = SQM,
           color = Weather)) +
  scale_x_reverse() +
  ylim(10, 17) +
  labs(title = "SQM vs Moon Brightness (Astronomical Scale)") +
  annotate("segment", x = -12.7, xend = -12.7, y = 10, yend = 10.7, 
           color = "black", linewidth = 1) +
  annotate("text", x = -12.7, y = 11, label = "Full Moon", 
           color ="black", angle = 90, hjust = 0, size = 3.5) +
  annotate("segment", x = -10, xend = -10, y = 10, yend = 10.7, 
           color = "black", linewidth = 1) +
  annotate("text", x = -10, y = 11, label = "First Quarter", 
           color ="black", angle = 90, hjust = 0, size = 3.5) +
  #  annotate("segment", x = -2.5, xend = -2.5, y = 10, yend = 11, 
  #           color = "black", linewidth = 1) +
  #  annotate("text", x = -2.5, y = 11.25, label = "New Moon", 
  #           color ="black", angle = 90, hjust = 0, size = 3.5) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), width = 0.075)


#apt:
ggplot(data = apt_moon_data,
       aes(x = Moon_Magnitude,
           y = SQM)) +
  scale_x_reverse() +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), width = 0.075)

ggplot(data = apt_moon_data,
       aes(x = Moon_Magnitude,
           y = SQM)) +
  scale_x_reverse() +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), width = 0.075)


ggplot(data = apt_moon_data,
       aes(x = Moon_Magnitude,
           y = SQM)) +
  scale_x_reverse() +
  ylim(10, 17) +
  labs(title = "SQM vs Moon Brightness (Astronomical Scale)") +
  annotate("segment", x = -12.7, xend = -12.7, y = 10, yend = 10.7, 
           color = "black", linewidth = 1) +
  annotate("text", x = -12.7, y = 11, label = "Full Moon", 
           color ="black", angle = 90, hjust = 0, size = 3.5) +
  annotate("segment", x = -10, xend = -10, y = 10, yend = 10.7, 
           color = "black", linewidth = 1) +
  annotate("text", x = -10, y = 11, label = "First Quarter", 
           color ="black", angle = 90, hjust = 0, size = 3.5) +
#  annotate("segment", x = -2.5, xend = -2.5, y = 10, yend = 11, 
#           color = "black", linewidth = 1) +
#  annotate("text", x = -2.5, y = 11.25, label = "New Moon", 
#           color ="black", angle = 90, hjust = 0, size = 3.5) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), width = 0.075)

apt_lm <- lm(SQM ~ Moon_Magnitude, data = apt_moon_data)


# Use moon under horizon (altitude < 0) as new moon as well. # put magnitude of new moon.
# See if there is any light pollution articles about influence of moon, especially if moon is below horizon.


#cmu: (kinda useless because there is not enough data at all.)
ggplot(data = cmu_moon_data,
       aes(x = Moon_Magnitude,
           y = SQM)) +
  scale_x_reverse() +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), width = 0.025)

ggplot(data = cmu_moon_data,
       aes(x = Moon_Magnitude,
           y = SQM)) +
  scale_x_reverse() +
  labs(title = "SQM vs Moon Brightness (Astronomical Scale)") +
  annotate("segment", x = -12.6, xend = -12.6, y = -Inf, yend = 1, 
           color = "black", linewidth = 1) +
  annotate("text", x = -12.6, y = 1.5, label = "Full Moon", 
           color ="black", angle = 90, hjust = 0, size = 3.5) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), width = 0.025)


# looking at time here

# There is simply not enough data; I'll have to pull an all-nighter or two.

# Use as_hms()

ggplot(data = apt_moon_data,
       aes(x = as_hms(Date_Time),
           y = SQM)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), width = 0.1)

ggplot(data = all_moon_data,
       aes(x = as_hms(Date_Time),
           y = SQM)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), width = 0.1)

ggplot(data = cmu_moon_data,
       aes(x = as_hms(Date_Time),
           y = SQM)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), width = 0.1)


names(apt_sqm_data)










