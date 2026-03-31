# This script will be used to do data analysis for the SQM at CMU specifically.
# Ethan Otto

# ==========================================
# 1. SETUP AND DATA IMPORT
# ==========================================
library(tidyverse)
library(lubridate)
library(hms)

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
      # NEW: Extract the hour and shift early morning readings to the previous calendar day
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
  
  # Remove impossible dark outliers right away
  # This is questionable...
  dat_filt <- dat_filt |> filter(SQM1 < 23, SQM2 < 23)
  
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
# -4.2584 -1.0601  0.0112  0.5951  3.4682 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      19.67216    0.37178  52.914  < 2e-16 ***
#   Moon_Magnitude    0.21205    0.02865   7.400 4.71e-13 ***
#   above_belowBelow  0.44451    0.17159   2.591  0.00982 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.628 on 589 degrees of freedom
# (108 observations deleted due to missingness)
# Multiple R-squared:  0.1373,	Adjusted R-squared:  0.1344 
# F-statistic: 46.87 on 2 and 589 DF,  p-value: < 2.2e-16


#


# > anova(all_moon_horiz_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq F value  Pr(>F)    
# Moon_Magnitude   1  230.72 230.723 87.0295 < 2e-16 ***
#   above_below      1   17.79  17.791  6.7109 0.00982 ** 
#   Residuals      589 1561.49   2.651                    
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
# -3.9495 -1.0145  0.0293  0.3855  3.7868 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       17.1107     0.1416 120.828  < 2e-16 ***
#   above_belowBelow   0.9739     0.1627   5.984 3.77e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.699 on 591 degrees of freedom
# (107 observations deleted due to missingness)
# Multiple R-squared:  0.05713,	Adjusted R-squared:  0.05553 
# F-statistic: 35.81 on 1 and 591 DF,  p-value: 3.775e-09

#

# > anova(all_moon_above_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# above_below   1  103.41 103.408  35.809 3.775e-09 ***
#   Residuals   591 1706.68   2.888                      
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
# -3.2085 -0.3737  0.1621  0.7797  1.9336 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         18.79745    0.18664 100.716 < 2e-16 ***
# Moon_Magnitude                       0.13143    0.01755   7.489 2.54e-13 ***
# CategoryGMO                          3.06890    0.45273   6.779 2.95e-11 ***
# CategoryRimrock                    219.30727 1287.10656   0.170  0.865    
# CategoryRock_Slide                  -2.37926    1.22171  -1.947 0.052 .  
# Moon_Magnitude:CategoryGMO          -0.04165    0.04781  -0.871 0.384    
# Moon_Magnitude:CategoryRimrock      18.14635  106.09686   0.171 0.864    
# Moon_Magnitude:CategoryRock_Slide   -0.07342    0.10652  -0.689 0.491    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9823 on 590 degrees of freedom
# (102 observations deleted due to missingness)
# Multiple R-squared:  0.6856,	Adjusted R-squared:  0.6819 
# F-statistic: 183.8 on 7 and 590 DF,  p-value: < 2.2e-16


# This model works well: R^2 = 0.6819 or makes up 68.19% of data.


# > anova(light_pollution_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq  F value Pr(>F)    
# Moon_Magnitude            1  231.41  231.41 239.8371 <2e-16 ***
#   Category                  3 1009.05  336.35 348.6059 <2e-16 ***
#   Moon_Magnitude:Category   3    1.15    0.38   0.3983 0.7542    
# Residuals               590  569.26    0.96                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lm_location_clean <- lm(SQM ~ Moon_Magnitude + Category, data = all_moon_data)
summary(lm_location_clean)

# Call:
#   lm(formula = SQM ~ Moon_Magnitude + Category, data = all_moon_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.1582 -0.4056  0.1608  0.7673  1.9912 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        18.72310    0.17223 108.709  < 2e-16 ***
#   Moon_Magnitude      0.12422    0.01611   7.712 5.24e-14 ***
#   CategoryGMO         3.45225    0.11611  29.731  < 2e-16 ***
#   CategoryRimrock    -0.84701    0.37465  -2.261   0.0241 *  
#   CategoryRock_Slide -1.55017    0.15905  -9.746  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9808 on 593 degrees of freedom
# (102 observations deleted due to missingness)
# Multiple R-squared:  0.685,	Adjusted R-squared:  0.6829 
# F-statistic: 322.4 on 4 and 593 DF,  p-value: < 2.2e-16


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






















