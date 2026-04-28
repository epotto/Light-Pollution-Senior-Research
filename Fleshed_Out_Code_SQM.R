# This script will be used to do data analysis for the SQM at CMU specifically.
# Ethan Otto

# ==========================================
# 1. SETUP AND DATA IMPORT
# ==========================================
library(tidyverse)
library(lubridate)
library(hms)
library(dplyr)

#data <- read.csv("SQM_Readings_for_R.csv", stringsAsFactors = FALSE)
data <- read.csv("Research_Data_Final.csv", stringsAsFactors = FALSE)

# ==========================================
# 2. FILTERING & CALCULATION FUNCTIONS
# ==========================================

names(data)

manual_filter <- function(data, interactive = FALSE) {
  if (interactive == TRUE) {
    print(names(data))
    max_col <- as.numeric(readline("Enter the max column number you would like to use (Exclude leading columns): "))
    
    while (max_col > length(names(data)) || is.na(max_col)) {
      print(paste("Invalid max column. Enter an integer less than or equal to:", length(names(data))))
      max_col <- as.numeric(readline("Enter the max column number you would like to use: "))
    }
  } else {
    max_col <- 29 
  }
  
  dat_filt <- data[, 1:max_col] |>
    mutate(
      Date_Time = parse_date_time(paste(Date, Time), orders = c("mdy_HMS", "mdy_HM", "ymd_HMS", "ymd_HM")),
      hour_of_day = hour(Date_Time), # Extracts the hour (0-23)
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
    # REMOVED: Relative_Humidity
    # ADDED: hour_of_day (so it doesn't get deleted during summarize)
    group_by(reading_group, Night_of, hour_of_day, Location, Date_Time, Category, 
             Weather_Conditions, Cloud_Cover, Clouds, Moon_Phase_Pcnt, 
             Moon_Alt_Calc, Moon_Az_Calc, Moon_Phase_Name, Phase_Angle_Calc,
             Moon_Magnitude, Extra_Variables, Sun_Alt_Calc) |>
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

extract_extra_variables <- function(data) {
  data |>
    mutate(
      Extra_Variables = replace_na(Extra_Variables, ""),
      is_baseline    = str_detect(Extra_Variables, "Baseline"),
      is_car_on      = str_detect(Extra_Variables, "Car_On"),
      greenhouse_on  = str_detect(Extra_Variables, "Greenhouse_On"),
      stadium_off    = str_detect(Extra_Variables, "Stadium_Off"),
      # NEW: Establishes your true dark skies
      true_dark_baseline = if_else(Category %in% c("GMO", "Highline_Lake", "Grand_Mesa", "Monument", "Moab"), TRUE, FALSE)
    )
}

moon_filter <- function(data, site_category = "All") {
  if (site_category != "All") {
    data <- data |> filter(Category == site_category)
  }
  
  clean_data <- data |>
    mutate(
      Moon_Phase_Pcnt = as.numeric(Moon_Phase_Pcnt),
      Moon_Magnitude = as.numeric(Moon_Magnitude),
      SQM            = as.numeric(SQM),
      variance       = as.numeric(variance),
      Moon_Alt_Calc  = as.numeric(Moon_Alt_Calc),
      
      above       = Moon_Alt_Calc > 0,
      above_below = ifelse(Moon_Alt_Calc > 0, "Above", "Below")
    )
  
  return(clean_data)
}

moon_func <- function(data) {
  data |>
    mutate(
      # Notice we are creating Moon_Phase_Num, leaving Moon_Phase_Name alone!
      Moon_Phase_Num = case_when(
        Moon_Phase_Name == "New_Moon" ~ 1,
        Moon_Phase_Name == "Waxing_Crescent" ~ 2,
        Moon_Phase_Name == "First_Quarter" ~ 3,
        Moon_Phase_Name == "Waxing_Gibbous" ~ 4,
        Moon_Phase_Name == "Full_Moon" ~ 5,
        Moon_Phase_Name == "Waning_Gibbous" ~ 6,
        Moon_Phase_Name %in% c("Last_Quarter", "Third Quarter") ~ 7, 
        Moon_Phase_Name == "Waning_Crescent" ~ 8,
        TRUE ~ NA_real_ 
      )
    )
}



# #data to only compare Campus vs. Baseline sites
# comparison_data <- all_moon_data |>
#   filter(Category == "CMU" | is_baseline == TRUE)


bortle_func <- function(data) {
  data |>
    mutate(
      Bortle_Class = case_when(
        SQM >= 22.00 ~ 1,
        SQM >= 21.90 ~ 2,
        SQM >= 21.70 ~ 3,
        SQM >= 20.50 ~ 4,
        SQM >= 19.50 ~ 5,
        SQM >= 18.95 ~ 6,
        SQM >= 18.38 ~ 7,
        SQM >= 17.80 ~ 8,
        SQM <  17.80 ~ 9,
        TRUE ~ NA_real_
      ),
      # Adding a text description makes plotting easier later!
      Bortle_Desc = case_when(
        Bortle_Class == 1 ~ "1: Pristine",
        Bortle_Class <= 3 ~ "2-3: Rural Dark",
        Bortle_Class <= 5 ~ "4-5: Suburban",
        Bortle_Class <= 7 ~ "6-7: Bright Suburban",
        Bortle_Class >= 8 ~ "8-9: City Light Dome"
      )
    )
}



# ==========================================
# 3. DATA PROCESSING EXECUTION
# ==========================================

sqm_mean <- manual_calculations(data, interactive = FALSE)
sqm_mean <- extract_extra_variables(sqm_mean) # Apply the new tags
sqm_mean <- moon_func(sqm_mean)
sqm_mean <- bortle_func(sqm_mean)

cmu_moon_data <- moon_filter(sqm_mean, "CMU")
rim_moon_data <- moon_filter(sqm_mean, "Rimrock")
apt_moon_data <- moon_filter(sqm_mean, "Rock_Slide")
all_moon_data <- moon_filter(sqm_mean, "All")


clean_moon_data <- subset(all_moon_data, Sun_Alt_Calc <= -18)

cat_avg <- aggregate(SQM ~ Category, data = clean_moon_data, FUN = mean)

summary_stats <- clean_moon_data %>%
  group_by(Category) %>%
  summarize(
    Mean_SQM = mean(SQM, na.rm = TRUE),
    Variance = mean(variance, na.rm = TRUE),          # <--- Here is your variance
    Standard_Error = sd(SQM, na.rm = TRUE) / sqrt(n())
  )



names(all_moon_data)

display_data <- all_moon_data |>
  reframe(Category, Location, Date_Time, SQM, variance, Night_of, hour_of_day, Moon_Magnitude, Moon_Phase_Pcnt, 
          is_baseline, is_car_on, Moon_Alt_Calc, Moon_Az_Calc, Moon_Phase_Name, true_dark_baseline, Sun_Alt_Calc,
          Bortle_Class, Bortle_Desc, above, above_below)



# ==========================================
# Weighted Non-Linear Least Squares (WNLS)
# ==========================================

# We will use nls() (Non-Linear Least Squares)
# This is the first, basic approach:

lunar_nls <- nls(SQM ~ above_below, data = clean_moon_data)
summary(lunar_nls)




# ==========================================
# Presentation stats!
# ==========================================

# Model 1: The Lunar Impact (Using full moon dataset)
lunar_lm <- lm(SQM ~ a + b * above, 
               data = clean_moon_data,
               start = list(a = 16, b = 1))
summary(lunar_lm)


lunar_wnls <- nls(SQM ~ base_sqm + Above * as.logical(above), 
                  data = clean_moon_data,
                  start = list(base_sqm = 16.59, Above = -1),
                  # This single line makes it a WNLS model
                  weights = variance)

summary(lunar_wnls)



# Fitting the non-linear opposition surge
ks_nls <- nls(SQM ~ base_sqm + phase_angle * (Phase_Angle_Calc^4), 
              data = clean_moon_data,
              start = list(base_sqm = 16.59, phase_angle = 0.0001),
              weights = variance)
summary(ks_nls)





new_moon_data <- sqm_mean %>% filter(Moon_Phase_Name == "New Moon")

summary(lm(SQM ~ true_dark_baseline, data = new_moon_data))


summary(lm(SQM ~ hour_of_day, data = clean_moon_data))

summary(lm(SQM ~ true_dark_baseline, data = clean_moon_data))



# Satellite: 

satellite_data <- aggregate(SQM ~ Category, data = clean_moon_data, FUN = mean)

clean_cmu_data <- clean_moon_data |>
  filter(Category == "CMU")

cmu_satellite_data <- aggregate(SQM ~ Location, data = clean_moon_data, FUN = mean)


library(ggplot2)
library(ggplot2)

# Scatter plot of SQM vs Moon Phase, with model lines automatically overlaid
ggplot(all_moon_data, aes(x = Moon_Magnitude, y = SQM, color = above_below)) +
  # The scatter plot of your cleaned moon data
  geom_point(alpha = 0.5, size = 2) + 
  
  # This single line automatically calculates and overlays the linear model (lm) lines 
  # for your groups without needing the raw coefficients
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  
  labs(
    title = "Impact of Moon Phase on Sky Brightness",
    x = "Moon Phase (%)",
    y = "Sky Quality Meter (SQM)",
    color = "Above/Below Category"
  ) +
  
  theme_minimal(base_size = 16) + 
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )


# Not relivant (adj R^2 0.1 less than moon_mag)

# phase_lunar_lm <- lm(SQM ~ Moon_Phase_Pcnt + above_below, data = all_moon_data)
# summary(phase_lunar_lm)

# mag_lunar_lm <- lm(SQM ~ Moon_Magnitude + above_below, data = all_moon_data)
# summary(mag_lunar_lm)

# This is not additive since it creates a paradox?

# Watch out for the sun! 

# ** Do NOT Let there be light :O **

# Keep only data where the sun is fully below -18 degrees (true astronomical night)
clean_moon_data <- subset(all_moon_data, Sun_Alt_Calc <= -18)


best_moon_lm <- lm(SQM ~ Moon_Magnitude * above_below, data = clean_moon_data)
summary(best_moon_lm)



summary(lm(SQM ~ Moon_Magnitude * Moon_Alt_Calc, data = clean_moon_data))


# Same day


# # Locations with multiple acquisitions per night:
consecutive_sqm <- clean_moon_data %>%
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


same_day_lm <- lm(SQM ~ Moon_Magnitude + Moon_Alt_Calc * Category, data = consecutive_sqm)
summary(same_day_lm)
anova(same_day_lm)






ggplot(data = all_moon_data, aes(x = Moon_Magnitude, y = SQM, color = above_below)) + 
  scale_x_reverse() +
  ylim(12, 17) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "grey40", width = 0.25) +
  
  # --- ADDED: The linear model overlay ---
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  
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




ggplot(data = clean_moon_data, 
       aes(x = Moon_Magnitude * above_below, 
           y = SQM)) +
  geom_point()


# Model 2: Pure light pollution (New Moon only)
# Filter for New Moon first
new_moon_data <- sqm_mean %>% filter(Moon_Cycle == "New_Moon")

light_dome_lm <- lm(SQM ~ true_dark_baseline, data = new_moon_data)
summary(light_dome_lm)


# Test A: Does the city get darker late at night?
time_lm <- lm(SQM ~ hour_of_day, data = clean_moon_data)
summary(time_lm)

# Test B: Does moon altitude matter?
altitude_lm <- lm(SQM ~ Moon_Alt_Calc, data = all_moon_data)
summary(altitude_lm)

# Model: Interaction between clouds and location
cloud_lm <- lm(SQM ~ Cloud_Cover + true_dark_baseline, data = all_moon_data)
summary(cloud_lm)


ggplot(data = all_moon_data, aes(x = as_hms(Date_Time), y = SQM)) +
  geom_point(size = 4, alpha = 0.7) + 
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "grey50", width = 300) +
  
  # --- ADDED: The linear model overlay (color set manually to stand out) ---
  geom_smooth(method = "lm", se = FALSE, color = "#F8766D", linewidth = 1.2) +
  
  scale_x_time(name = "Time of Night", breaks = hms::hms(hours = c(0, 4, 8, 12, 16, 20, 24))) +
  coord_cartesian(ylim = c(14, 22)) + 
  labs(title = "SQM Readings Over Time (All Locations)",
       subtitle = "Data grouped by time of acquisition") +
  theme_minimal()

ggplot(clean_moon_data, aes(sample = SQM)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "Normal Q-Q Plot", x = "Theoretical", y = "Sample") +
  theme_minimal()


# Generates just the Scale-Location plot
plot(location_lunar_lm, which = 3, 
     main = "Scale-Location: Variance Check")


# Install and load the lmtest package if you haven't already
# install.packages("lmtest")
library(lmtest)


# Run the Breusch-Pagan test for homoscedasticity
bptest(location_lunar_lm)




ggplot(data = all_moon_data, aes(x = Moon_Magnitude, y = SQM, color = above_below)) + 
  scale_x_reverse() +
  ylim(12, 17) +
  geom_point(size = 4.5) +
  geom_errorbar(aes(ymin = SQM - variance, ymax = SQM + variance), color = "grey40", width = 0.25) +
  
  # --- ADDED: The linear model overlay ---
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  
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




# Let's get more sophisticated! 

complex_lunar_lm <- lm(SQM ~ Moon_Phase_Pcnt + Moon_Alt_Calc + above_below, data = clean_moon_data)
summary(complex_lunar_lm)

# This shows Moon_Alt_Calc isn't worth keeping for any interaction!

location_lunar_lm <- lm(SQM ~ Moon_Magnitude + Moon_Alt_Calc * Category, data = clean_moon_data)
summary(location_lunar_lm)

# This is the best we got so far! A decent representation given the nuance.

# Test if the hour of the night changes the SQM, and if that depends on whether you are in the city
time_of_night_lm <- lm(SQM ~ hour_of_day + true_dark_baseline, data = clean_moon_data)
summary(time_of_night_lm)


# ==========================================
# 4. STATISTICAL MODELING
# ==========================================



# Isolate only the moments the moon is physically visible
moon_up_data <- all_moon_data |> 
  filter(above_below == "Above")

# Test if altitude and magnitude interact while the moon is up
moon_extinction_lm <- lm(SQM ~ Moon_Magnitude * Moon_Alt_Calc + true_dark_baseline, data = moon_up_data)
summary(moon_extinction_lm)



# Get the New Moon data to remove lunar interference
new_moon_data <- all_moon_data |> 
  filter(Moon_Cycle == "New_Moon")

# Test if the hour of the night changes the SQM, and if that depends on whether you are in the city
time_of_night_lm <- lm(SQM ~ hour_of_day * true_dark_baseline, data = new_moon_data)
summary(time_of_night_lm)


# Does the moon setting push a site from a Bortle 8 to a Bortle 7?
bortle_lm <- lm(Bortle_Class ~ above_below + true_dark_baseline, data = all_moon_data)
summary(bortle_lm)






# Assuming 'Clouds' is a text category like "Clear" vs "Cloudy"
summary(lm(SQM ~ Cloud_Cover * true_dark_baseline, data = new_moon_data))



# ==========================================
# 1. REDEFINING THE TRUE DARK SKY BASELINE
# ==========================================
# Instead of relying on the manual tags in Extra_Variables, 
# let's hardcode the true remote sites as your baseline.

sqm_mean <- sqm_mean |>
  mutate(
    # True if it's a deeply dark site, False if it's urban/suburban
    true_dark_baseline = if_else(Category %in% c("GMO", "Highland_Lake", "Grand_Mesa", "Monument", "Moab"), 
                                 TRUE, FALSE)
  )


# 1. Ensure you have the New Moon data filtered
new_moon_data <- sqm_mean |> filter(Moon_Cycle == "New_Moon")

# 2. Run the linear regression model
pure_light_dome_lm <- lm(SQM ~ true_dark_baseline, data = new_moon_data)

# 3. View the results to get your Estimates and p-values
summary(pure_light_dome_lm)




# Create a dataset where the moon doesn't exist
new_moon_data <- sqm_mean |>
  filter(Moon_Cycle == "New_Moon")

# Run a very clean model on this data. 
# Notice we dropped all moon variables because they are irrelevant here!
pure_light_dome_lm <- lm(SQM ~ Category, data = new_moon_data)

# View the results to see the true difference between sites without the moon
summary(pure_light_dome_lm)






ggplot(new_moon_data, aes(x = reorder(Category, -SQM), y = SQM, fill = true_dark_baseline)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("FALSE" = "#E69F00", "TRUE" = "#56B4E9"),
                    labels = c("Light Polluted (Valley)", "True Dark Sky (Baseline)")) +
  theme_minimal() +
  labs(
    title = "Pure Light Pollution Comparison (New Moon Data Only)",
    subtitle = "With the moon removed, all differences are due to geographic location",
    x = "Location Category",
    y = expression("SQM (mag/arcsec"^2*")"),
    fill = "Site Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(clean_moon_data, aes(x = reorder(Category, -SQM), y = SQM, fill = true_dark_baseline)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("FALSE" = "#E69F00", "TRUE" = "#56B4E9"),
                    labels = c("Light Polluted (Valley)", "True Dark Sky (Baseline)")) +
  theme_minimal() +
  labs(
    title = "Pure Light Pollution Comparison (All Moon Data)",
    subtitle = "With the moon removed, all differences are due to geographic location",
    x = "Location Category",
    y = expression("SQM (mag/arcsec"^2*")"),
    fill = "Site Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sqm_outlier <- filter(all_moon_data, SQM > 22)


boxplot_data <- clean_moon_data |>
  filter(Category != "Moab" & Category != "Grand_Mesa" & Category != "Connected_Lakes")


ggplot(boxplot_data, aes(x = reorder(Category, -SQM), y = SQM, fill = true_dark_baseline)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("FALSE" = "#E69F00", "TRUE" = "#56B4E9"),
                    labels = c("Light Polluted (Valley)", "True Dark Sky (Baseline)")) +
  theme_minimal() +
  labs(
    title = "Pure Light Pollution Comparison (All Moon Data)",
    subtitle = "With the moon removed, all differences are due to geographic location",
    x = "Location Category",
    y = expression("SQM (mag/arcsec"^2*")"),
    fill = "Site Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sqm_outlier <- filter(all_moon_data, SQM > 22)



all_moon_horiz_lm <- lm(SQM ~ Moon_Magnitude + above_below, data = all_moon_data)
summary(all_moon_horiz_lm)
anova(all_moon_horiz_lm)





all_moon_above_lm <- lm(SQM ~ above_below, data = all_moon_data)
summary(all_moon_above_lm)
anova(all_moon_above_lm)




# Test for above_below with category. 
# Next step would be doing a classification. Maybe use bortle scale


category_above_lm <- lm(formula = SQM ~ Moon_Magnitude + above_below*Category, data = all_moon_data)
summary(category_above_lm)






# Testing for Moon_Altitude * Moon_Magnitude:

category_above_lm <- lm(formula = SQM ~ (Moon_Magnitude*Moon_Altitude) + (above_below*Category), data = all_moon_data)
summary(category_above_lm)
anova(category_above_lm)





# Testing if the Moon's effect changes depending on the Location
light_pollution_lm <- lm(SQM ~ Moon_Magnitude * Category, data = all_moon_data)

summary(light_pollution_lm)
anova(light_pollution_lm)





lm_location_clean <- lm(SQM ~ Moon_Magnitude + Category, data = all_moon_data)
summary(lm_location_clean)




 
 
 # We control for the moon so we isolate purely the location's light pollution
 baseline_lm <- lm(SQM ~ Moon_Magnitude + above_below + is_baseline, data = comparison_data)
 summary(baseline_lm)
 
 

 


# ==========================================
# 4.2 Same Day
# ========================================

same_day_lm <- lm(SQM ~ Moon_Magnitude + Category, data = consecutive_sqm)
summary(same_day_lm)
anova(same_day_lm)



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





ggplot(comparison_data, aes(x = is_baseline, y = SQM, fill = is_baseline)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_reverse() + # Reverse so darker skies (higher SQM) are at the top
  scale_fill_manual(values = c("FALSE" = "#E69F00", "TRUE" = "#56B4E9"), 
                    labels = c("Campus (Light Dome)", "Baseline (Dark Sky)")) +
  scale_x_discrete(labels = c("FALSE" = "CMU Campus", "TRUE" = "Baseline Sites")) +
  theme_minimal() +
  labs(
    title = "Quantifying the Light Dome Effect",
    x = "Site Designation",
    y = expression("SQM (mag/arcsec"^2*")"),
    fill = "Site Type"
  )







# Filtering for New Moon to isolate the Light Dome
new_moon_data <- sqm_mean %>% filter(Moon_Cycle == "New_Moon")

ggplot(new_moon_data, aes(x = reorder(Category, -SQM), y = SQM, fill = true_dark_baseline)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_reverse() + # Standard for SQM to show "up" as darker
  scale_fill_manual(values = c("FALSE" = "#E69F00", "TRUE" = "#56B4E9"),
                    labels = c("Light Polluted (Valley)", "True Dark Sky (Baseline)")) +
  theme_minimal(base_size = 15) +
  labs(
    title = "Pure Light Pollution: New Moon Comparison",
    subtitle = "All differences shown are due to location, not the moon",
    x = "Location Category",
    y = expression("SQM (mag/arcsec"^2*")"),
    fill = "Site Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save it for your LaTeX project
ggsave("new_moon_boxplot.png", width = 8, height = 5)


















