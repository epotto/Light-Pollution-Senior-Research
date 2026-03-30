# This script will be used to do data analysis for the SQM at CMU specifically.
# Ethan Otto

# ==========================================
# 1. SETUP AND DATA IMPORT
# ==========================================
library(tidyverse)
library(lubridate)
library(hms)

# ETHAN NOTE: Always a good idea to read strings as characters rather than factors
data <- read.csv("SQM_Readings_for_R.csv", stringsAsFactors = FALSE)


# ==========================================
# 2. FILTERING & CALCULATION FUNCTIONS
# ==========================================

manual_filter <- function(data, interactive = FALSE) {
  # ETHAN NOTE: Swapped your 'i' for 'interactive' so it's clearer what TRUE/FALSE means.
  if (interactive == TRUE) {
    print(names(data))
    max_col <- as.numeric(readline("Enter the max column number you would like to use (Exclude leading columns): "))
    
    while (max_col > length(names(data)) || is.na(max_col)) {
      print(paste("Invalid max column. Enter an integer less than or equal to:", length(names(data))))
      max_col <- as.numeric(readline("Enter the max column number you would like to use: "))
    }
  } else {
    max_col <- 27 # Subject to change based on Excel variables
  }
  
  dat_filt <- data[, 1:max_col] |>
    # ETHAN NOTE: Moved group_by out of here, as it doesn't do anything without a summarise() or mutate() after it.
    mutate(Date_Time = mdy_hms(paste(Date, Time))) |>
    filter(!is.na(Date_Time))
  
  return(dat_filt)
}


names(data)

manual_calculations <- function(data, interactive = FALSE) {
  # ETHAN NOTE: This function replaces n_count, math_func, and avg_between_2_SQM.
  # Instead of using for-loops to jump every 6 rows, we just create a "group_id" 
  # that changes every 6 rows, and let summarise() do all the math instantly.
  
  dat_filt <- manual_filter(data, interactive)
  
  sqm_mean <- dat_filt |>
    # Create a grouping variable that increments every 6 rows
    mutate(reading_group = (row_number() - 1) %/% 6) |>
    
    # Group by everything we want to KEEP, plus our new 6-row chunk ID
    group_by(reading_group, Category, Location, Date_Time, Weather_Conditions, Cloud_Cover, 
             Clouds, Relative_Humidity, Moon_Brightness, Moon_Cycle, Moon_Magnitude, 
             Moon_Altitude, Extra_Variables) |>
    
    # Calculate the means and standard errors for each chunk of 6 automatically
    summarise(
      SQM1_mean = mean(SQM1, na.rm = TRUE),
      SQM2_mean = mean(SQM2, na.rm = TRUE),
      
      # sum(!is.na()) automatically counts how many non-NA values exist (replaces your n_count func)
      sd1 = sd(SQM1, na.rm = TRUE) / sqrt(sum(!is.na(SQM1))),
      sd2 = sd(SQM2, na.rm = TRUE) / sqrt(sum(!is.na(SQM2))),
      .groups = "drop"
    ) |>
    
    # Finally, average the two SQMs together just like your old function did
    mutate(
      SQM = (SQM1_mean + SQM2_mean) / 2,
      variance = (sd1 + sd2) / 2
    )
  
  return(sqm_mean)
}


moon_filter <- function(data, site_category = "All") {
  # ETHAN NOTE: Updated this to use the category name directly instead of 1, 2, 3, 4.
  # Also incorporated the vectorization logic we discussed earlier.
  
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
  # ETHAN NOTE: Replaced the 8 'if/else' statements with case_when().
  # It checks the Moon_Cycle column and outputs the correct number instantly.
  data |>
    mutate(
      Phase_Num = case_when(
        Moon_Cycle == "New_Moon" ~ 1,
        Moon_Cycle == "Waxing_Crescent" ~ 2,
        Moon_Cycle == "First_Quarter" ~ 3,
        Moon_Cycle == "Waxing_Gibbous" ~ 4,
        Moon_Cycle == "Full_Moon" ~ 5,
        Moon_Cycle == "Waning_Gibbous" ~ 6,
        Moon_Cycle %in% c("Last_Quarter", "Third Quarter") ~ 7, # %in% handles the "or" logic
        Moon_Cycle == "Waning_Crescent" ~ 8,
        TRUE ~ NA_real_ # Catch-all for missing data
      )
    )
}

# ==========================================
# 3. DATA PROCESSING EXECUTION
# ==========================================

# Calculate the 6-row averages
sqm_mean <- manual_calculations(data, interactive = FALSE)

# Generate Phase Numbers
sqm_moon_mean <- moon_func(sqm_mean)

# Generate Location-Specific Dataframes
cmu_moon_data <- moon_filter(sqm_moon_mean, "CMU")
rim_moon_data <- moon_filter(sqm_moon_mean, "Rimrock")
apt_moon_data <- moon_filter(sqm_moon_mean, "Rock_Slide")
all_moon_data <- moon_filter(sqm_moon_mean, "All")

# ==========================================
# 4. STATISTICAL MODELING
# ==========================================

# Linear model: Moon magnitude and above/below horizon
all_moon_horiz_lm <- lm(SQM ~ Moon_Magnitude * above_below, data = all_moon_data)
summary(all_moon_horiz_lm)
anova(all_moon_horiz_lm)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.8568 -0.6129 -0.0973  0.6346  5.8953 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                      10.1830     5.0736   2.007   0.0473 *
#   Moon_Magnitude                   -0.4645     0.4078  -1.139   0.2572  
# above_belowBelow                  3.3057     5.3049   0.623   0.5345  
# Moon_Magnitude:above_belowBelow   0.1939     0.4306   0.450   0.6533  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.056 on 107 degrees of freedom
# (297 observations deleted due to missingness)
# Multiple R-squared:  0.09841,	Adjusted R-squared:  0.07313 
# F-statistic: 3.893 on 3 and 107 DF,  p-value: 0.01102




# Notes on above model:
# - No relationship between moon magnitude and SQM.
# - There IS a relationship between the Moon being above/below the horizon and SQM. 
# - No relationship between moon magnitude and moon above/below.
# - R^2 = 0.07313 (7.313% of variance explained. Not very good even for ENVS).

# Linear model: Above Horizon Only
all_moon_above_lm <- lm(SQM ~ above_below, data = all_moon_data)
summary(all_moon_above_lm)
anova(all_moon_above_lm)

# ==========================================
# 5. VISUALIZATIONS
# ==========================================

# 5.1 LM Diagnostics Plots
ggplot(all_moon_horiz_lm, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residuals vs Fitted (Mostly evenly distributed)")

ggplot(all_moon_horiz_lm, aes(sample = SQM)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot (Normal distribution confirmed)")

# 5.2 Main Plot: All Locations
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

# 5.3 Plot: Apartments Only
ggplot(data = apt_moon_data, aes(x = Moon_Magnitude, y = SQM, color = above_below)) +
  scale_x_reverse() +
  ylim(12, 17) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "grey40", width = 0.4) +
  labs(
    title = "SQM vs Moon Brightness (Apartment Only)",
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

# 5.4 Time Series Plot (Using as_hms)
ggplot(data = all_moon_data, aes(x = as_hms(Date_Time), y = SQM)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "black", width = 0.1) +
  scale_x_time(name = "Time of Night") + # scale_x_time is built specifically for hms objects!
  labs(title = "SQM Readings Over Time (All Locations)")















