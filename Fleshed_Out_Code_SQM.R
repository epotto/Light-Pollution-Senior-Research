# This script will be used to do data analysis for the SQM at CMU specifically.
# Ethan Otto

# ==========================================
# 1. SETUP AND DATA IMPORT
# ==========================================
library(tidyverse)
library(lubridate)
library(hms)
library(dplyr)

data <- read.csv("SQM_Readings_for_R.csv", stringsAsFactors = FALSE)

# ==========================================
# 2. FILTERING & CALCULATION FUNCTIONS
# ==========================================

manual_filter <- function(data, interactive = FALSE) {
  if (interactive == TRUE) {
    print(names(data))
    max_col <- as.numeric(readline("Enter the max column number you would like to use (Exclude leading columns): "))
    
    while (max_col > length(names(data)) || is.na(max_col)) {
      print(paste("Invalid max column. Enter an integer less than or equal to:", length(names(data))))
      max_col <- as.numeric(readline("Enter the max column number you would like to use: "))
    }
  } else {
    max_col <- 27 
  }
  
  dat_filt <- data[, 1:max_col] |>
    mutate(
      Date_Time = mdy_hms(paste(Date, Time)),
      hour_of_day = hour(Date_Time),
      Night_of = if_else(hour_of_day < 12, 
                         as.Date(Date_Time) - days(1), 
                         as.Date(Date_Time))
    ) |>
    filter(!is.na(Date_Time))
  
  return(dat_filt)
}

manual_calculations <- function(data, interactive = FALSE) {
  dat_filt <- manual_filter(data, interactive)
  
  sqm_mean <- dat_filt |>
    mutate(reading_group = (row_number() - 1) %/% 6) |>
    # NEW: Added Night_of to the group_by so it passes through to the final data
    group_by(reading_group, Night_of, Location, Date_Time, Category, Weather_Conditions, Cloud_Cover, 
             Clouds, Relative_Humidity, Moon_Brightness, Moon_Cycle, Moon_Magnitude, 
             Moon_Altitude, Extra_Variables) |>
    summarise(
      SQM1_mean = mean(SQM1, na.rm = TRUE),
      SQM2_mean = mean(SQM2, na.rm = TRUE),
      sd1 = sd(SQM1, na.rm = TRUE) / sqrt(sum(!is.na(SQM1))),
      sd2 = sd(SQM2, na.rm = TRUE) / sqrt(sum(!is.na(SQM2))),
      .groups = "drop"
    ) |>
    mutate(
      SQM = (SQM1_mean + SQM2_mean) / 2,
      variance = (sd1 + sd2) / 2
    )
  
  return(sqm_mean)
}

# NEW FUNCTION: Extracts specific tags from the Extra_Variables string
extract_extra_variables <- function(data) {
  data |>
    mutate(
      Extra_Variables = replace_na(Extra_Variables, ""),
      is_baseline    = str_detect(Extra_Variables, "Baseline"),
      is_car_on      = str_detect(Extra_Variables, "Car_On"),
      greenhouse_on  = str_detect(Extra_Variables, "Greenhouse_On"),
      stadium_off    = str_detect(Extra_Variables, "Stadium_Off")
    )
}

moon_filter <- function(data, site_category = "All") {
  if (site_category != "All") {
    data <- data |> filter(Category == site_category)
  }
  
  clean_data <- data |>
    mutate(
      Moon_Magnitude = as.numeric(Moon_Magnitude),
      SQM            = as.numeric(SQM),
      variance       = as.numeric(variance),
      Moon_Altitude  = as.numeric(Moon_Altitude),
      
      above       = Moon_Altitude > 0,
      above_below = ifelse(Moon_Altitude > 0, "Above", "Below")
    )
  
  return(clean_data)
}

moon_func <- function(data) {
  data |>
    mutate(
      Phase_Num = case_when(
        Moon_Cycle == "New_Moon" ~ 1,
        Moon_Cycle == "Waxing_Crescent" ~ 2,
        Moon_Cycle == "First_Quarter" ~ 3,
        Moon_Cycle == "Waxing_Gibbous" ~ 4,
        Moon_Cycle == "Full_Moon" ~ 5,
        Moon_Cycle == "Waning_Gibbous" ~ 6,
        Moon_Cycle %in% c("Last_Quarter", "Third Quarter") ~ 7, 
        Moon_Cycle == "Waning_Crescent" ~ 8,
        TRUE ~ NA_real_ 
      )
    )
}

# Locations with multiple acquisitions per night:
consecutive_sqm <- all_moon_data %>%
  # 1. Group by your location and the specific night
  group_by(Category, Location, Night_of) %>%
  
  # 2. Filter to keep only groups with more than 1 distinct time entry
  filter(n_distinct(Date_Time) > 1) %>%
  
  # 3. Ungroup so future operations aren't affected by the grouping
  ungroup() %>%
  
  # 4. Optional: Sort the data so it's easy to look at the matching nights side-by-side
  arrange(Category, Night_of, Date_Time)

# View the first few rows to confirm it worked
head(consecutive_sqm)



# ==========================================
# 3. DATA PROCESSING EXECUTION
# ==========================================

sqm_mean <- manual_calculations(data, interactive = FALSE)
sqm_mean <- extract_extra_variables(sqm_mean) # Apply the new tags
sqm_mean <- moon_func(sqm_mean)

cmu_moon_data <- moon_filter(sqm_mean, "CMU")
rim_moon_data <- moon_filter(sqm_mean, "Rimrock")
apt_moon_data <- moon_filter(sqm_mean, "Rock_Slide")
all_moon_data <- moon_filter(sqm_mean, "All")

# ==========================================
# 4. STATISTICAL MODELING
# ==========================================



all_moon_horiz_lm <- lm(SQM ~ Moon_Magnitude + above_below, data = all_moon_data)
summary(all_moon_horiz_lm)
anova(all_moon_horiz_lm)


# Call:
#   lm(formula = SQM ~ Moon_Magnitude + above_below, data = all_moon_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.6475 -1.0969  0.0055  0.6897  3.5472 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      19.75641    0.37558  52.603  < 2e-16 ***
#   Moon_Magnitude    0.21812    0.02898   7.528 1.82e-13 ***
#   above_belowBelow  0.30739    0.17106   1.797   0.0728 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.651 on 621 degrees of freedom
# (89 observations deleted due to missingness)
# Multiple R-squared:  0.1221,	Adjusted R-squared:  0.1192 
# F-statistic: 43.17 on 2 and 621 DF,  p-value: < 2.2e-16


#

# > anova(all_moon_horiz_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq F value  Pr(>F)    
# Moon_Magnitude   1  226.46 226.459 83.1060 < 2e-16 ***
#   above_below      1    8.80   8.799  3.2292 0.07282 .  
# Residuals      621 1692.19   2.725                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#



all_moon_above_lm <- lm(SQM ~ above_below, data = all_moon_data)
summary(all_moon_above_lm)
anova(all_moon_above_lm)


# Call:
#   lm(formula = SQM ~ above_below, data = all_moon_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.7250 -1.1067  0.0633  0.4913  3.7699 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       17.1201     0.1415 120.974  < 2e-16 ***
#   above_belowBelow   0.8466     0.1620   5.226 2.37e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.722 on 623 degrees of freedom
# (88 observations deleted due to missingness)
# Multiple R-squared:  0.042,	Adjusted R-squared:  0.04046 
# F-statistic: 27.31 on 1 and 623 DF,  p-value: 2.366e-07

#

# > anova(all_moon_above_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# above_below   1   80.95  80.953  27.311 2.366e-07 ***
#   Residuals   623 1846.63   2.964                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Test for above_below with category. 
# Next step would be doing a classification. Maybe use bortle scale


category_above_lm <- lm(formula = SQM ~ Moon_Magnitude + above_below*Category, data = all_moon_data)
summary(category_above_lm)

Call:
  lm(formula = SQM ~ Moon_Magnitude + above_below * Category, data = all_moon_data)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.5391 -0.4016  0.1520  0.5910  2.2168 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         16.75338    0.22347  74.970  < 2e-16 ***
#   Moon_Magnitude                       0.03837    0.01636   2.346 0.019292 *  
#   above_belowBelow                     1.36552    0.11188  12.205  < 2e-16 ***
#   CategoryGMO                          4.35017    0.18434  23.599  < 2e-16 ***
#   CategoryRimrock                     -1.63314    0.16251 -10.049  < 2e-16 ***
#   CategoryRock_Slide                  -0.77538    0.21510  -3.605 0.000338 ***
#   above_belowBelow:CategoryGMO        -0.90723    0.22278  -4.072 5.26e-05 ***
#   above_belowBelow:CategoryRimrock          NA         NA      NA       NA    
# above_belowBelow:CategoryRock_Slide -0.93289    0.29414  -3.172 0.001591 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.892 on 616 degrees of freedom
# (89 observations deleted due to missingness)
# Multiple R-squared:  0.7457,	Adjusted R-squared:  0.7428 
# F-statistic: 258.1 on 7 and 616 DF,  p-value: < 2.2e-16


# Testing for Moon_Altitude * Moon_Magnitude:

category_above_lm <- lm(formula = SQM ~ (Moon_Magnitude*Moon_Altitude) + (above_below*Category), data = all_moon_data)
summary(category_above_lm)
anova(category_above_lm)

# Call:
#   lm(formula = SQM ~ (Moon_Magnitude * Moon_Altitude) + (above_below * 
#                                                            Category), data = all_moon_data)
# 
# # Residuals:
# #   Min      1Q  Median      3Q     Max 
# # -3.7109 -0.3646  0.1157  0.5696  2.0045 
# # 
# # Coefficients: (1 not defined because of singularities)
# # Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)                         18.863630   0.511235  36.898  < 2e-16 ***
# #   Moon_Magnitude                       0.174429   0.033853   5.152 3.47e-07 ***
# #   Moon_Altitude                        0.050919   0.011180   4.555 6.33e-06 ***
# #   above_belowBelow                     0.648542   0.191490   3.387 0.000752 ***
# #   CategoryGMO                          3.722608   0.227499  16.363  < 2e-16 ***
# #   CategoryRimrock                     -1.578396   0.160615  -9.827  < 2e-16 ***
# #   CategoryRock_Slide                  -0.959675   0.215650  -4.450 1.02e-05 ***
# #   Moon_Magnitude:Moon_Altitude         0.004819   0.001056   4.562 6.13e-06 ***
# #   above_belowBelow:CategoryGMO        -0.138214   0.276294  -0.500 0.617084    
# # above_belowBelow:CategoryRimrock           NA         NA      NA       NA    
# # above_belowBelow:CategoryRock_Slide -0.544597   0.301851  -1.804 0.071691 .  
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # 
# # Residual standard error: 0.8786 on 614 degrees of freedom
# # (89 observations deleted due to missingness)
# # Multiple R-squared:  0.7541,	Adjusted R-squared:  0.7505 
# # F-statistic: 209.2 on 9 and 614 DF,  p-value: < 2.2e-16


# > anova(category_above_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq  F value    Pr(>F)    
# Moon_Magnitude                 1  226.46  226.46 293.3800 < 2.2e-16 ***
#   Moon_Altitude                  1    2.16    2.16   2.8015  0.094687 .  
# above_below                    1    7.55    7.55   9.7854  0.001842 ** 
#   Category                       3 1183.13  394.38 510.9178 < 2.2e-16 ***
#   Moon_Magnitude:Moon_Altitude   1   31.69   31.69  41.0521 2.953e-10 ***
#   above_below:Category           2    2.51    1.26   1.6280  0.197170    
# Residuals                    614  473.95    0.77                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Testing if the Moon's effect changes depending on the Location
light_pollution_lm <- lm(SQM ~ Moon_Magnitude * Category, data = all_moon_data)

summary(light_pollution_lm)
anova(light_pollution_lm)


# Call:
#   lm(formula = SQM ~ Moon_Magnitude * Category, data = all_moon_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2083 -0.3871  0.1587  0.7672  1.9336 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       18.79745    0.18867  99.633  < 2e-16 ***
#   Moon_Magnitude                     0.13148    0.01774   7.411 4.14e-13 ***
#   CategoryGMO                        3.08412    0.45765   6.739 3.67e-11 ***
#   CategoryRimrock                   -4.44906    2.06677  -2.153   0.0317 *  
#   CategoryRock_Slide                -2.37926    1.23498  -1.927   0.0545 .  
# Moon_Magnitude:CategoryGMO        -0.03889    0.04833  -0.805   0.4213    
# Moon_Magnitude:CategoryRimrock    -0.29790    0.19731  -1.510   0.1316    
# Moon_Magnitude:CategoryRock_Slide -0.07346    0.10768  -0.682   0.4953    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9929 on 616 degrees of freedom
# (89 observations deleted due to missingness)
# Multiple R-squared:  0.6849,	Adjusted R-squared:  0.6813 
# F-statistic: 191.3 on 7 and 616 DF,  p-value: < 2.2e-16


# This model works well: R^2 = 0.6819 or makes up 68.13% of data.


# > anova(light_pollution_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq  F value Pr(>F)    
# Moon_Magnitude            1  226.46  226.46 229.6926 <2e-16 ***
#   Category                  3 1090.48  363.49 368.6827 <2e-16 ***
#   Moon_Magnitude:Category   3    3.18    1.06   1.0764 0.3585    
# Residuals               616  607.33    0.99                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lm_location_clean <- lm(SQM ~ Moon_Magnitude + Category, data = all_moon_data)
summary(lm_location_clean)

# Call:
#   lm(formula = SQM ~ Moon_Magnitude + Category, data = all_moon_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.1500 -0.3878  0.1614  0.7634  1.9898 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        18.70632    0.17385 107.603  < 2e-16 ***
#   Moon_Magnitude      0.12264    0.01625   7.546 1.62e-13 ***
#   CategoryGMO         3.44485    0.11756  29.302  < 2e-16 ***
#   CategoryRimrock    -1.34115    0.17897  -7.494 2.33e-13 ***
#   CategoryRock_Slide -1.55138    0.16105  -9.633  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9931 on 619 degrees of freedom
# (89 observations deleted due to missingness)
# Multiple R-squared:  0.6833,	Adjusted R-squared:  0.6812 
# F-statistic: 333.8 on 4 and 619 DF,  p-value: < 2.2e-16



 lm_location_with_cat <- lm(SQM ~ Moon_Magnitude + Category * Location, data = all_moon_data)
 summary(lm_location_with_cat)
 anova(lm_location_with_cat)
 
 # >  anova(lm_location_with_cat)
 # Analysis of Variance Table
 # 
 # Response: SQM
 # Df  Sum Sq Mean Sq F value    Pr(>F)    
 # Moon_Magnitude   1  226.46  226.46 402.322 < 2.2e-16 ***
 #   Category         3 1090.48  363.49 645.773 < 2.2e-16 ***
 #   Location        46  287.98    6.26  11.122 < 2.2e-16 ***
 #   Residuals      573  322.53    0.56                      
 # ---
 #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




# ==========================================
# 4.2 Same Day
# ========================================

same_day_lm <- lm(SQM ~ Moon_Magnitude + Category, data = consecutive_sqm)
summary(same_day_lm)

# Call:
#   lm(formula = SQM ~ Moon_Magnitude + Category, data = consecutive_sqm)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.8150 -0.2470  0.3751  0.5458  0.8942 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        17.997737   0.197955  90.918  < 2e-16 ***
#   Moon_Magnitude      0.009805   0.023982   0.409    0.683    
# CategoryGMO         3.066348   0.184042  16.661  < 2e-16 ***
#   CategoryRock_Slide -2.012424   0.310114  -6.489 1.17e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8491 on 151 degrees of freedom
# (34 observations deleted due to missingness)
# Multiple R-squared:  0.7211,	Adjusted R-squared:  0.7155 
# F-statistic: 130.1 on 3 and 151 DF,  p-value: < 2.2e-16


# ==========================================
# 5. VISUALIZATIONS
# ==========================================


# Statistics


# lm(SQM ~ Moon_Magnitude + above_below, data = all_moon_data):

ggplot(all_moon_horiz_lm, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residuals vs Fitted (Not evenly distributed)")

# Contrary to what the code says now, this is most definitely not an even distribution.

ggplot(all_moon_horiz_lm, aes(sample = SQM)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot (Not Normal distribution confirmed)")


# This is clearly not a straight line.


# lm(SQM ~ above_below, data = all_moon_data):

ggplot(all_moon_above_lm, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residuals vs Fitted (Not evenly distributed)")

# This is just a pair of parallel lines.

ggplot(all_moon_above_lm, aes(sample = SQM)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot (Not Normal distribution confirmed)")


# Sinusoidal?


#lm(SQM ~ Moon_Magnitude * Category, data = all_moon_data):


ggplot(light_pollution_lm, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residuals vs Fitted (Not evenly distributed)")

# This is a bunch of dots, but largley are on the bottom of the line. (Not evenly distributed.)

ggplot(light_pollution_lm, aes(sample = SQM)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot (Not Normal distribution confirmed)")

# Sinusoidal again it seems






#####
# General Data:

ggplot(data = all_moon_data, aes(x = Moon_Magnitude, y = SQM, color = above_below)) + 
  scale_x_reverse() +
  ylim(12, 17) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "grey40", width = 0.25) +
  labs(
    title = "SQM vs Moon Brightness (All Locations)",
    color = "Moon Above Horizon", 
    x = "Moon Magnitude", 
    y = expression("SQM (mag/arcsec"^2*")")
  ) +
  annotate("segment", x = -12.7, xend = -12.7, y = 12.5, yend = 12, color = "black", linewidth = 1) +
  annotate("text", x = -12.3, y = 13.0, label = "Full Moon", color ="black", angle = 0, hjust = 0, size = 3.5) +
  annotate("segment", x = -10, xend = -10, y = 12.5, yend = 12, color = "black", linewidth = 1) +
  annotate("text", x = -9.5, y = 13.0, label = "First Quarter", color ="black", angle = 0, hjust = 0, size = 3.5) +
  annotate("segment", x = -6, xend = -6, y = 12.5, yend = 12, color = "black", linewidth = 1) +
  annotate("text", x = -5.3, y = 13.0, label = "Crescent Moon", color ="black", angle = 0, hjust = 0, size = 3.5)

ggplot(data = all_moon_data, aes(x = as_hms(Date_Time), y = SQM)) +
  geom_point(size = 4, alpha = 0.7) + 
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "grey50", width = 300) +
  scale_x_time(name = "Time of Night", breaks = hms::hms(hours = c(0, 4, 8, 12, 16, 20, 24))) +
  coord_cartesian(ylim = c(14, 22)) + 
  labs(title = "SQM Readings Over Time (All Locations)",
       subtitle = "Data grouped by time of acquisition") +
  theme_minimal()


ggplot(data = all_moon_data, 
       aes(x = Moon_Magnitude, y = SQM, color = Category, fill = Category)) +
  
  # Reverse X so Full Moon (brighter) is on the left
  scale_x_reverse() + 
  
  # Plot the actual data points (made them a bit transparent)
  geom_point(size = 3, alpha = 0.5) +
  
  # Draw the linear regression lines for EACH category automatically
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  
  theme_minimal() +
  labs(
    title = "Lunar Impact on Sky Quality by Location",
    subtitle = "Testing if light pollution washes out the moon's effect",
    x = "Moon Magnitude (Dimmer ->)",
    y = expression("SQM (mag/arcsec"^2*")"),
    color = "Location",
    fill = "Location"
  )


# ==========================================
# 6. BASELINE VS CAMPUS COMPARISON (NEW)
# ==========================================

# 6.1 The Global Comparison Boxplot
ggplot(sqm_mean, aes(x = Category, y = SQM, fill = Category)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_reverse() + 
  theme_minimal() +
  labs(
    title = "The Light Dome Effect: Campus vs. Baseline Skies",
    subtitle = "Comparing Sky Quality across the Grand Valley",
    x = "Observation Category",
    y = expression("SQM (mag/arcsec"^2*")")
  )

# 6.2 The Delta SQM Calculator
calculate_light_dome_effect <- function(data) {
  data |>
    # Group by the specific observation night
    group_by(Night_of) |>
    summarise(
      avg_campus   = mean(SQM[Category == "CMU"], na.rm = TRUE),
      avg_baseline = mean(SQM[Category %in% c("Rimrock", "Rock_Slide")], na.rm = TRUE)
    ) |>
    # Drop nights where we didn't measure BOTH campus and baseline
    filter(!is.na(avg_campus) & !is.na(avg_baseline)) |>
    mutate(
      # Calculate how much darker the baseline is
      sqm_difference = avg_baseline - avg_campus 
    )
}

# Run the calculation to see the results
light_dome_delta <- calculate_light_dome_effect(sqm_mean)
print(light_dome_delta)


# # A tibble: 3 × 4
# Night_of   avg_campus avg_baseline sqm_difference
#    <date>          <dbl>        <dbl>          <dbl>
# 1 2026-03-02       15.9         14.4          -1.58
# 2 2026-03-04       17.1         14.9          -2.25
# 3 2026-03-20       18.1         16.0          -2.15






















