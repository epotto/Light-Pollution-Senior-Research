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
#variable_processer <- function(data){
#  i <- readline(prompt = "Which variable set? (Remember to use 'data' for raw_var and 'sqm_mean_all_data' for var and #moon_var)
#                input 1 for raw, 2 for all usefull var, or 3 for moon_var")
#  if(i ==1){
#    dat_filt <- data[,1:17]
#    return(names(dat_filt))
#  }else if(i ==2){
#    return(names(data))
#  }else if(i ==3){
#    moon_filt <- data |>
#      select(Location, Date, Time, Category, SQM, Moon_Cycle, Phase., 
#             Moon_Brightness, Moon_Magnitude, Moon_Altitude)
#    return(names(moon_filt))
#  }
#}

#raw_var <- variable_processer(data)
#moon_var <- variable_processer(sqm_mean_all_data)


# Testing:

dat_filt <- data[,1:18] |>
  group_by(Location,Date,Time, Category) |>
  mutate(Date_Time = mdy_hms(paste(Date,Time))) |>
  filter(!is.na(Date_Time))
date_time <- dat_filt |>
  ungroup() |>
  group_by(Date, Time) |>
  summarise(Date = unique(Date)) |>
  filter(Date != "") |> 
  mutate(Date_Time = mdy_hms(paste(Date,Time))) 

date_time <- dat_filt |>
  distinct(Date_Time, .keep_all = TRUE)


sqm1_mean <- dat_filt |>
  aggregate(SQM1 ~ Date_Time,
            FUN = mean, na.rm = TRUE) 

sqm2_mean <- dat_filt |>
  aggregate(SQM2 ~ Date_Time,
            FUN = mean, na.rm = TRUE) 

variance <- aggregate(SQM1~ Date_Time,
                      dat_filt,
                      FUN = sd, na.rm = TRUE) |>
  reframe(variance = SQM1 / sqrt(6)) |>
  select(variance)

variance2 <- aggregate(SQM2~ Date_Time,
                      dat_filt,
                      FUN = sd, na.rm = TRUE) |>
  reframe(variance2 = SQM2 / sqrt(6)) |>
  select(variance2)

pre_sqm_means <- mutate(sqm1_mean,sqm2_mean)
total_variance <- sqrt(variance^2 + variance2^2 + 2*(0.1)^2)/2
sqm_means_data <- mutate(total_variance, pre_sqm_means)

# Now to join the mean of each SQM device.

only_sqm_means <- sqm_means_data |>
  mutate(SQM = rowMeans(c(SQM1,SQM2)))

sqm_mean <- right_join(sqm_means_data, date_time)|>
  mutate(SQM = rowMeans(only_sqm_means)) |>
  group_by(Category,Date_Time) 
#reframe(Date_Time,Category,Location,SQM, variance,
#        Weather,Cloud_Cover,Relative_Humidity,
#        Moon_Brightness,Moon_Cycle, Moon_Magnitude, Moon_Altitude)






names(data)

# Basic processer works to give mean of all right now.
# I also added a function to find standard deviation.

basic_processer <- function(data){
  dat_filt <- data[,1:18] |>
    group_by(Location,Date,Time, Category) |>
    mutate(Date_Time = mdy_hms(paste(Date,Time))) |>
    filter(!is.na(Date_Time))
  date_time <- dat_filt |>
    distinct(Date_Time, .keep_all = TRUE)
  
  sqm1_mean <- aggregate(SQM1~ Date_Time, 
                         dat_filt,
                         FUN = mean, na.rm = TRUE)
  
  sqm2_mean <- aggregate(SQM2~ Date_Time, 
                         dat_filt,
                         FUN = mean, na.rm = TRUE) 
    
  variance <- aggregate(SQM1~ Date_Time,
                        dat_filt,
                        FUN = sd, na.rm = TRUE) |>
    reframe(variance = SQM1 / sqrt(6)) |>
    select(variance)
  
  variance2 <- aggregate(SQM2~ Date_Time,
                         dat_filt,
                         FUN = sd, na.rm = TRUE) |>
    reframe(variance2 = SQM2 / sqrt(6)) |>
    select(variance2)
    # There is one NA, so this isn't perfect... I will have to see if I should change that.
    # I think I'll have to make a for loop instead. It isn't using built in functions of course, but it would be better most likely.
    
  pre_sqm_means <- mutate(sqm1_mean,sqm2_mean)
  total_variance <- sqrt(variance^2 + variance2^2 + 2*(0.1)^2)/2
  sqm_means_data <- mutate(total_variance, pre_sqm_means)
  
  # Now to join the mean of each SQM device.
  
  only_sqm_means <- sqm_means_data |>
    select(SQM1,SQM2)
  
  sqm_mean <- right_join(sqm_means_data, date_time)|>
    mutate(SQM = rowMeans(only_sqm_means)) |>
    group_by(Category,Date_Time) 
    #reframe(Date_Time,Category,Location,SQM, variance,
    #        Weather,Cloud_Cover,Relative_Humidity,
    #        Moon_Brightness,Moon_Cycle, Moon_Magnitude, Moon_Altitude)
}

sqm_mean_all_data <- basic_processer(data)





# Slightly broken old one:

# This just needs the inputs all put in, but it breaks it because it isn't doing it properly.
basic_processer <- function(data){
  dat_filt <- data[,1:17] |>
    group_by(Location,Date,Time, Category) 
  date_time <- dat_filt |>
    ungroup() |>
    group_by(Date, Time) |>
    summarise(Date = unique(Date)) |>
    filter(Date != "") |> 
    mutate(Date_Time = mdy_hms(paste(Date,Time))) 
  dat_datetime <- right_join(dat_filt,date_time)
  
  sqm1_mean <- dat_datetime |>
    aggregate(SQM1~ Time + Date + Location + Category, 
              FUN = mean, na.rm = TRUE) |>
    
    sqm2_mean <- dat_datetime |>
    aggregate(SQM2~ Time + Date + Location + Category, 
              FUN = mean, na.rm = TRUE) |>
    ungroup() |>
    reframe(Date,Time,Category,Location,SQM2,
            Weather_Conditions,Clouds,Cloud_Cover,Relative_Humidity,
            Moon_Brightness,Moon_Cycle, Moon_Magnitude, Moon_Altitude)
  variance <- aggregate(SQM1~ Time + Date + Location + Category + 
                          Weather_Conditions + Clouds + Relative_Humidity + Cloud_Cover + 
                          Moon_Brightness + Moon_Cycle + Moon_Magnitude + Moon_Altitude,dat_filt, 
                        FUN = sd, na.rm = TRUE) |>
    reframe(variance = SQM1 / sqrt(6)) |>
    select(variance)
  variance2 <- aggregate(SQM2~ Time + Date + Location + Category + 
                           Weather_Conditions + Clouds + Clouds + Relative_Humidity + Cloud_Cover + 
                           Moon_Brightness + Moon_Cycle + Moon_Magnitude + Moon_Altitude,dat_filt, 
                         FUN = sd, na.rm = TRUE) |>
    reframe(variance2 = SQM2 / sqrt(6)) |> 
    # There is one NA, so this isn't perfect... I will have to see if I should change that.
    select(variance2)
  
  pre_sqm_means <- right_join(sqm1_mean,sqm2_mean)
  total_variance <- sqrt(variance^2 + variance2^2 + 2*(0.1)^2)/2
  sqm_means_data <- mutate(total_variance, pre_sqm_means)
  
  # Now to join the mean of each SQM device.
  
  only_sqm_means <- sqm_means_data |>
    select(SQM1,SQM2)
  
  sqm_mean <- right_join(sqm_means_data, date_time)|>
    reframe(Category, SQM = rowMeans(only_sqm_means)) |>
    group_by(Category,Date_Time) |>
    reframe(Date_Time,Category,Location,SQM, variance,
            Weather,Cloud_Cover,Relative_Humidity,
            Moon_Brightness,Moon_Cycle, Moon_Magnitude, Moon_Altitude)
}

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
      dataset[i] <- "Above"
    } else{
      dataset[i] <- "Below"
    }
  }
  return(dataset)
}

moon_above <- function(data){
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
      mutate(above = moon_above(cmu_moon_data)) |>
      mutate(above_below = moon_above_or_below(cmu_moon_data))
    return(cmu_moon_data)
  } else if(i ==2){
    rim_moon_data <- data |>
      filter(Category == "Rimrock")
    rim_moon_data <- rim_moon_data |>
      mutate(above = moon_above(rim_moon_data)) |>
    mutate(above_below = moon_above_or_below(rim_moon_data))
    return(rim_moon_data)
  } else if(i ==3){
    apt_moon_data <- data |>
      filter(Category == "Rock_Slide")
    apt_moon_data <- apt_moon_data |>
      mutate(above = moon_above(apt_moon_data)) |>
      mutate(above_below = moon_above_or_below(apt_moon_data))
    return(apt_moon_data)
  } else if(i ==4){
    all_moon_data <- data |>
      mutate(above = moon_above(data)) |>
      mutate(above_below = moon_above_or_below(data))
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
  ylim(12, 17) +
  labs(title = "SQM vs Moon Brightness (All Locations)") +
  annotate("segment", x = -12.7, xend = -12.7, y = 12.5, yend = 12, 
           color = "black", linewidth = 1) +
  annotate("text", x = -12.3, y = 13.0, label = "Full Moon", 
           color ="black", angle = 0, hjust = 0, size = 3.5) +
  annotate("segment", x = -10, xend = -10, y = 12.5, yend = 12, 
           color = "black", linewidth = 1) +
  annotate("text", x = -9.5, y = 13.0, label = "First Quarter", 
           color ="black", angle = 0, hjust = 0, size = 3.5) +
  annotate("segment", x = -6, xend = -6, y = 12.5, yend = 12, 
           color = "black", linewidth = 1) +
  annotate("text", x = -5.3, y = 13.0, label = "Crescent Moon", 
           color ="black", angle = 0, hjust = 0, size = 3.5) +
  #  annotate("segment", x = -2.5, xend = -2.5, y = 10, yend = 11, 
  #           color = "black", linewidth = 1) +
  #  annotate("text", x = -2.5, y = 11.25, label = "New Moon", 
  #           color ="black", angle = 0, hjust = 0, size = 3.5) +
  #  annotate("segment", x = -2.5, xend = -2.5, y = 10, yend = 11, 
  #           color = "black", linewidth = 1) +
  #  annotate("text", x = -2.5, y = 11.25, label = "New Moon", 
  #           color ="black", angle = 0, hjust = 0, size = 3.5) +
  labs(color = "Moon Above Horizon", x = "Moon Magnitude", y = expression("SQM (mag/arcsec"^2*")")) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "grey40", width = 0.25)


# linear model with respect to moon_magnitude and whether it is above or below horizon.

all_moon_horiz_lm <- lm(SQM ~ Moon_Magnitude * as.character(above_below), all_moon_data)

summary(all_moon_horiz_lm)


ggplot(all_moon_horiz_lm,
       aes(x = .fitted,
           y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

# This indicates that the data is mostly evenly distributed since the points are distributed somewhat evenly across the x-axis and have roughly the same magnitude on both sides of the x-axis.

ggplot(all_moon_horiz_lm,
       aes(sample = SQM)) +
  stat_qq() +
  stat_qq_line()

# This indicates that the data has normal distribution since the data points all align closely to the produced line.


# Thus, it is normal and has equal variance.


anova(all_moon_horiz_lm)

#                                          Df Sum Sq Mean Sq F value    Pr(>F)
#Moon_Magnitude                            1  0.077  0.0766  0.1318    0.7179
#as.character(above_below)                 1 19.343 19.3435 33.2878 3.259e-07
#Moon_Magnitude:as.character(above_below)  1  0.553  0.5532  0.9520    0.3333
#Residuals                                58 33.704  0.5811               



# No relationship between moon magnitude and SQM.

# There is a relationship between the Moon being above/below the horizon and SQM. 

# No relationship between moon magnitude and moon above/below.

# The latitude coefficient evaluates if there is a relationship between latitude and cab size, which is the case (F_1,390 = 208.55, p < 0.001).

# R^2 = 0.3396, which means that 33.96% of the data points are represented by the line. This isn't great, but for ENVS it is acceptable.

summary(all_moon_horiz_lm)



# Above only:


all_moon_above_lm <- lm(SQM ~ as.character(above_below), all_moon_data)

summary(all_moon_above_lm)

anova(all_moon_above_lm)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.6640 -0.5939  0.1179  0.6047  1.4202 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                     15.5033     0.1568  98.869  < 2e-16 ***
#  as.character(above_below)Below   1.0073     0.2058   4.895 7.75e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.7996 on 60 degrees of freedom
#Multiple R-squared:  0.2854,	Adjusted R-squared:  0.2735 
#F-statistic: 23.96 on 1 and 60 DF,  p-value: 7.748e-06


# Intercept isn't zero.

# Above and below do have a significant impact.


ggplot(data = apt_moon_data,
       aes(x = Moon_Magnitude,
           y = SQM,
           color = Weather)) +
  scale_x_reverse() +
  ylim(12, 17) +
  labs(title = "SQM vs Moon Brightness (Astronomical Scale)") +
  annotate("segment", x = -12.7, xend = -12.7, y = 12.5, yend = 12, 
           color = "black", linewidth = 1) +
  annotate("text", x = -12.3, y = 13.0, label = "Full Moon", 
           color ="black", angle = 0, hjust = 0, size = 3.5) +
  annotate("segment", x = -10, xend = -10, y = 12.5, yend = 12, 
           color = "black", linewidth = 1) +
  annotate("text", x = -9.5, y = 13.0, label = "First Quarter", 
           color ="black", angle = 0, hjust = 0, size = 3.5) +
  annotate("segment", x = -6, xend = -6, y = 12.5, yend = 12, 
           color = "black", linewidth = 1) +
  annotate("text", x = -5.3, y = 13.0, label = "Crescent Moon", 
           color ="black", angle = 0, hjust = 0, size = 3.5) +
  #  annotate("segment", x = -2.5, xend = -2.5, y = 10, yend = 11, 
  #           color = "black", linewidth = 1) +
  #  annotate("text", x = -2.5, y = 11.25, label = "New Moon", 
  #           color ="black", angle = 0, hjust = 0, size = 3.5) +
  #  annotate("segment", x = -2.5, xend = -2.5, y = 10, yend = 11, 
  #           color = "black", linewidth = 1) +
  #  annotate("text", x = -2.5, y = 11.25, label = "New Moon", 
  #           color ="black", angle = 0, hjust = 0, size = 3.5) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "grey40", width = 0.4)


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
           y = SQM,
           color = as.logical(above))) +
  scale_x_reverse() +
  ylim(12, 17) +
  labs(title = "SQM vs Moon Brightness (Apartment Only)") +
  annotate("segment", x = -12.7, xend = -12.7, y = 12.5, yend = 12, 
           color = "black", linewidth = 1) +
  annotate("text", x = -12.3, y = 13.0, label = "Full Moon", 
           color ="black", angle = 0, hjust = 0, size = 3.5) +
  annotate("segment", x = -10, xend = -10, y = 12.5, yend = 12, 
           color = "black", linewidth = 1) +
  annotate("text", x = -9.5, y = 13.0, label = "First Quarter", 
           color ="black", angle = 0, hjust = 0, size = 3.5) +
  annotate("segment", x = -6, xend = -6, y = 12.5, yend = 12, 
           color = "black", linewidth = 1) +
  annotate("text", x = -5.3, y = 13.0, label = "Crescent Moon", 
           color ="black", angle = 0, hjust = 0, size = 3.5) +
  #  annotate("segment", x = -2.5, xend = -2.5, y = 10, yend = 11, 
  #           color = "black", linewidth = 1) +
  #  annotate("text", x = -2.5, y = 11.25, label = "New Moon", 
  #           color ="black", angle = 0, hjust = 0, size = 3.5) +
  #  annotate("segment", x = -2.5, xend = -2.5, y = 10, yend = 11, 
  #           color = "black", linewidth = 1) +
  #  annotate("text", x = -2.5, y = 11.25, label = "New Moon", 
  #           color ="black", angle = 0, hjust = 0, size = 3.5) +
  labs(color = "Moon Above Horizon", x = "Moon Magnitude", y = expression("SQM (mag/arcsec"^2*")")) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "grey40", width = 0.4)

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
  scale_x_continuous(breaks = 12 + seq(0,24,4), name = "time", 
                     labels = c("12:00:00", "16:00:00", "20:00:00", "00:00:00", "4:00:00", "8:00:00", "12:00:00")) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "black", width = 0.1)

ggplot(data = all_moon_data,
       aes(x = as_hms(Date_Time),
           y = SQM)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "white", width = 0.1)

ggplot(data = cmu_moon_data,
       aes(x = as_hms(Date_Time),
           y = SQM)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "white", width = 0.1)


ggplot(data = all_moon_data,
       aes(x = as_hms(Date_Time),
           y = SQM)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "white", width = 0.1)










