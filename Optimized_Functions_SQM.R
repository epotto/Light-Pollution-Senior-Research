# This script will be used to do data analysis for the SQM at CMU specifically.

# Ethan Otto


library(tidyverse)
library(lubridate)
library(dplyr)
library(lubridate)
library(hms)

data <- read.csv("SQM_Readings_for_R.csv")

names(data)

# Use ctrl + shift + c to comment many selected lines.

# In this document, I'm going to start getting the loops to be more "manual" in a sense. I'm going to use more for loops specific to the methods I have set, that way I don't have to learn so many niche functions that are subject to change.

# This is going to begin using readline(prompt = " ") to get information from user once functions are called.



# I would like to use "Extra_Variables" as a sort of junk column. I want to just have a string that can be read with if statements so I don't have to make columns for hyper-specific purposes. (i.e. Delta Field having a greenhouse and stadium light input)



manual_filter <- function(data, i){
  # Start by getting for loops and the like to be able to do this manually.
  if(i == TRUE){
    print(names(data))
    max_col <- readline("Enter the max column number you would like to use (Please exclude leading columns): ")
    # Making sure that going from 1:max_col will not cause an error:
    if(max_col > length(names(data))){
      while(max_col > length(names(data))){
        print(paste("Invalid max column. Enter an integer less than or equal to the maximum number of columns; ", length(names(data))))
        max_col <- readline("Enter the max column number you would like to use (Please exclude leading columns): ")
      } } 
  } else{
    max_col <- 27 # 27 is subject to change; this is just the max number of variables I have in Excel at this time. This may increase or decrease based on future needs!
  }
  dat_filt <- data[,1:max_col] |>
    group_by(Location,Date,Time, Category) |>
    mutate(Date_Time = mdy_hms(paste(Date,Time))) |>
    filter(!is.na(Date_Time))
  return(dat_filt)
  }

dat_filt <- manual_filter(data, FALSE)


# Attempting to get the averages and stuff manually for faster computation.

# Correct code outside of function: 


# This should work:

pts_per_instance <- seq(from = 1, to = nrow(dat_filt), by = 6)
date_time <- dat_filt[pts_per_instance,]


avg1 <- c()

 sqm1_mean <- for(i in 1:nrow(date_time)){
  avg1[i] = mean(dat_filt$SQM1[((6*(i-1)) + 1: (6*(i-1)+ 6))]) # This will suck to interpret later, but it is just going from 1 to 6, and taking into account how many iterations there already have been (6*(i-1))
}

i <- 1


mutate(date_time, SQM = avg1)

mean(dat_filt$SQM1[1:6])

mean(dat_filt$SQM1[((6*(i-1)) + 1) : (6*(i-1) + 6)])

nrow(dat_filt) / 6


sqm1_mean

is.na(date_time$SQM2[414])


# Functions:


n_count <- function(data, n, i, func){
  n <- as.integer(n)
  i <- as.integer(i)
  n_count <- 6
  i_count <- n*(i-1)
  if(func == "SQM1"){
    for(i in 1:6){
      if(is.na(data$SQM1[i_count + i] == TRUE)){
        n_count <- n_count - 1
      }
    }
  } else if(func == "SQM2"){
    for(i in 1:6){
      if(is.na(data$SQM2[i_count + i] == TRUE)){
        n_count <- n_count - 1
      }
    }
  }
  return(n_count)
}


avg_func <- function(data, date_time, func){
  avg <- c()
  if(func == "mean"){
    for(i in 1:nrow(date_time)){
      avg[[i]] <- (as.numeric(data$SQM1[i]) + as.numeric(data$SQM2[i]))/ 2 
    } } else if(func == "sd"){ # for the variance/sd stuff:
      for(i in 1:nrow(date_time)){
        # Need to put in more accurate error:
        #reframe(variance2 = SQM2 / sqrt(6)) |>
        # total_variance <- sqrt(variance^2 + variance2^2 + 2*(0.1)^2)/2
        avg[[i]] <- (as.numeric(data$sd1[i]) + as.numeric(data$sd2[i]))/ 2 
      } }
  return(avg)
}

avg <- (as.numeric(sqm_means$SQM1[1]) + as.numeric(sqm_means$SQM2[1]))/ 2 



# function for inputting lists to mutate()

mutate_func <- function(date_time, n, func){
  n <- as.integer(n)
  n_sd <- 0
  avg <- c()
  if(func == 1){
    for(i in 1:nrow(date_time)){
      avg[[i]] <-  mean(dat_filt$SQM1[((n*(i-1)) + 1: (n*(i-1)+ n))], na.rm = TRUE) 
      }
    }else if(func ==2){
      for(i in 1:nrow(date_time)){
        avg[[i]] <- mean(dat_filt$SQM2[((n*(i-1)) + 1: (n*(i-1)+ n))], na.rm = TRUE) }
      
    }else if(func ==3){
      for(i in 1:nrow(date_time)){
        i <- as.integer(i)
        n_sd <- n_count(dat_filt, n, i, "SQM1")
        avg[[i]] <- sd(dat_filt$SQM1[((n*(i-1)) + 1: (n*(i-1)+ n))], na.rm = TRUE) / sqrt(n_sd)}
    }else if(func ==4){
      for(i in 1:nrow(date_time)){
        i <- as.integer(i)
        n_sd <- n_count(dat_filt, n, i, "SQM2")
        avg[[i]] <- sd(dat_filt$SQM2[((n*(i-1)) + 1: (n*(i-1)+ n))], na.rm = TRUE) / sqrt(n_sd)}
    }
  return(avg)
}



#Putting avg and sd into manual_calculations:

manual_calculations <- function(data){
  dat_filt <- manual_filter(data, FALSE)
  n <- 6
  pts_per_instance <- seq(from = 1, to = nrow(dat_filt), by = 6)
  date_time <- dat_filt[pts_per_instance,]
  
  date <- date_time |>
    reframe(Category, Location, Date_Time)
  
  # The code below works!
  
  sqm_means <- date |>
    mutate(SQM1 = mutate_func(date_time,6,1)) |>
    mutate(SQM2 = mutate_func(date_time,6,2)) |>
    mutate(sd1 = mutate_func(date_time,6,3)) |>
    mutate(sd2 = mutate_func(date_time,6,4))
  #return(sqm_means)
  
  sqm_mean <- date |>
    mutate(SQM = avg_func(sqm_means, date_time, "mean")) |>
    mutate(variance = avg_func(sqm_means, date_time, "sd"))
  #return(sqm_mean)
  
  date_time1 <- date_time |>
    select(- SQM1, - SQM2)
  
   sqm_total_info <- right_join(date_time1, sqm_mean)
   #return(sqm_total_info)
}

#reframe(variance2 = SQM2 / sqrt(6)) |>
# total_variance <- sqrt(variance^2 + variance2^2 + 2*(0.1)^2)/2

sqm_mean <- manual_calculations(data)



# Create a sample data frame
df <- data.frame(
  ID = c(1, 2, 3),
  Name = c("Aliyana", "Boby", "Charlie"),
  Skills = c("R,Python,SQL", "Excel,Tableau", "Java,C++,Python")
)
print(df)

# Split the Skills column using strsplit
skills_split <- strsplit(df$Skills, ",")
# Create a new data frame with repeated rows based on the length of split lists
df_long <- df[rep(seq_len(nrow(df)), sapply(skills_split, length)), ]
# Assign the expanded skills to the new data frame
df_long$Skills <- unlist(skills_split)
print(df_long)



# This is a test to see what I can do with Extra_Variables column.

# I'm going to try using the "separate_rows()" function

test_text <- sqm_mean$Extra_Variables[222]

comma_split_test <- strsplit(test_text, ",")
df_long <- test_text[rep(seq_len(nrow(test_text)), sapply(comma_split_test, length)), ]


sqm_mean$Extra_Variables[222]

if(sqm_mean$Extra_Variables[[222]] == "Car_On"){
  return(TRUE)
}


read.csv(text = test_text)

read.csv(text = sqm_mean$Extra_Variables[222])

date_time <- date_time |>
  select(- SQM1, - SQM2)
sqm_total_info <- right_join(date_time, sqm_mean)










sqm_mean <- right_join(sqm1,sqm2)

# The code below works!

sqm_mean <- date |>
  mutate(SQM1 = t(mutate_func(date_time,6,1))) |>
  mutate(SQM2 = t(mutate_func(date_time,6,2))) |>
  select(SQM1, SQM2) 

sqm_mean_total <- date |>
  mutate(SQM = avg_func(sqm_mean, date_time, "mean"))


manual_calculations_old_func <- function(data){
  # default <- readline("Would you like to use default settings? (Press y/n)")
  # if(default == "n"){
  #   dat_filt <- manual_filter(data, TRUE)
  #   n <- as.numeric(readline("How many data points per aquisition instance?"))
  #   pts_per_instance <- seq(from = 1, to = nrow(dat_filt), by = n)
  # } else{
    dat_filt <- manual_filter(data, FALSE)
    n <- 6
    pts_per_instance <- seq(from = 1, to = nrow(dat_filt), by = 6)
  #}
  date_time <- dat_filt[pts_per_instance,]
  print(date_time)
  # Now let's start getting the means!
  #avg1 <- vector(mode = "list", length = nrow(date_time))
  #avg2 <- vector(mode = "list", length = nrow(date_time))
  #print(avg2)
  # This may suck to interpret later, but it is just going from 1 to 6, and taking into account how many iterations there already have been (6*(i-1))
  
  sqm_mean <- date_time |>
    reframe(SQM = mutate_func(date_time,n,1))
    #mutate(SQM1 = mutate_func(date_time,1))
    #mutate(SQM2 = mutate_func(date_time,2))
}

sqm_mean <- manual_calculations(data)


#Testing stuff:

func_test <- function(data){
  avg <- vector(mode = "list", length = nrow(date_time1))
  print(length(avg))
  n <- 6
  for(i in 1:nrow(date_time1)){
    avg[[i]] = mean(dat_filt$SQM1[((n*(i-1)) + 1: (n*(i-1)+ n))], na.rm = TRUE) 
  }
  print(avg)
}

func_test(dat_filt)

sqm_mean <- manual_calculations(data)



manual_processer(data)











  dataset <- list()
  for(i in 1:length(data$Moon_Altitude)){
    if(is.na(data$Moon_Altitude[i]) == TRUE){
      dataset[i] <- NA
    } else{
      if(data$Moon_Altitude[i] > 0){
        dataset[i] <- TRUE
        } else{
          dataset[i] <- FALSE
        }
  }
  return(dataset)
}
#reframe(Date_Time,Category,Location,SQM, variance,
#        Weather,Cloud_Cover,Relative_Humidity,
#        Moon_Brightness,Moon_Cycle, Moon_Magnitude, Moon_Altitude)












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

cmu_sqm_data <- location_distinguisher(sqm_mean,1)
rim_sqm_data <- location_distinguisher(sqm_mean,2)
apt_sqm_data <- location_distinguisher(sqm_mean,3)


# This will filter out data that isn't useful for conclusions on the effect of the moon.

is.na(sqm_mean$Moon_Altitude[200])

moon_above_or_below <- function(data){ # This function is mostly for R to plot as a legend.
  dataset <- list()
  for(i in 1:length(data$Moon_Altitude)){
    if(is.na(data$Moon_Altitude[i]) == TRUE){
      dataset[i] <- NA
    } else{
      if(data$Moon_Altitude[i] > 0){
        dataset[i] <- "Above"
      } else{
        dataset[i] <- "Below"
      }
    }
  }
  return(t(dataset))
}

moon_above <- function(data){ # This is where R gives a logical to Moon_Altitude so we can easily manipulate it.
  dataset <- list()
  for(i in 1:length(data$Moon_Altitude)){
    if(is.na(data$Moon_Altitude[i]) == TRUE){
      dataset[i] <- NA
    } else{
      if(data$Moon_Altitude[i] > 0){
        dataset[i] <- TRUE
        } else{
          dataset[i] <- FALSE
        }
    }
  }
  return(t(dataset))
}



moon_filter <- function(data, i){
  pts_per_instance <- seq(from = 1, to = nrow(dat_filt), by = 6)
  date_time <- dat_filt[pts_per_instance,]
  if(i == 1){
    cmu_moon_filt <- date_time |>
      filter(Category == "CMU") 
    cmu_moon_data <- cmu_moon_filt |>
      mutate(above = moon_above(cmu_moon_data)) |> 
      mutate(above_below = moon_above_or_below(cmu_moon_data))
    
    return(cmu_moon_data)
  } else if(i ==2){
    rim_moon_data <- date_time |>
      filter(Category == "Rimrock")
    rim_moon_data <- rim_moon_data |>
      mutate(above = moon_above(rim_moon_data)) |>
      mutate(above_below = moon_above_or_below(rim_moon_data))
    return(rim_moon_data)
  } else if(i ==3){
    apt_moon_data <- date_time |>
      filter(Category == "Rock_Slide")
    apt_moon_data <- apt_moon_data |>
      mutate(above = moon_above(apt_moon_data)) |>
      mutate(above_below = moon_above_or_below(apt_moon_data))
    return(apt_moon_data)
  } else if(i ==4){
    all_moon_data <- date_time |>
      mutate(above = moon_above(data)) |>
      mutate(above_below = moon_above_or_below(data))
    return(all_moon_data)
  }
}

cmu_moon_data <- moon_filter(sqm_mean,1)
rim_moon_data <- moon_filter(sqm_mean,2)
apt_moon_data <- moon_filter(sqm_mean,3)
all_moon_data <- moon_filter(sqm_mean,4)

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










