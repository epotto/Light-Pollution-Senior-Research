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








