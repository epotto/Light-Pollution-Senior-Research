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
             Moon_Altitude, Moon_Distance, Extra_Variables) |>
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
      Moon_Distance  = as.numeric(Moon_Distance),
      
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


#data to only compare Campus vs. Baseline sites
comparison_data <- all_moon_data |>
  filter(Category == "CMU" | is_baseline == TRUE)




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
# -5.0147 -1.2364 -0.0010  0.8723  3.6452 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       20.3870     0.5191  39.276  < 2e-16 ***
#   Moon_Magnitude     0.2939     0.0399   7.367 9.78e-13 ***
#   above_belowBelow   0.7877     0.2373   3.319 0.000984 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.787 on 408 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.2107,	Adjusted R-squared:  0.2068 
# F-statistic: 54.45 on 2 and 408 DF,  p-value: < 2.2e-16


#

# > anova(all_moon_horiz_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# Moon_Magnitude   1  312.55 312.552  97.877 < 2.2e-16 ***
#   above_below      1   35.18  35.180  11.017  0.000984 ***
#   Residuals      408 1302.87   3.193                      
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
# -5.1087 -1.2179 -0.1904  0.7824  4.0624 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       16.8209     0.1991   84.48  < 2e-16 ***
#   above_belowBelow   1.5762     0.2255    6.99 1.12e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.899 on 411 degrees of freedom
# Multiple R-squared:  0.1063,	Adjusted R-squared:  0.1041 
# F-statistic: 48.86 on 1 and 411 DF,  p-value: 1.116e-11

#

# > anova(all_moon_above_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# above_below   1  176.26 176.259  48.862 1.116e-11 ***
#   Residuals   411 1482.61   3.607                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Test for above_below with category. 
# Next step would be doing a classification. Maybe use bortle scale


category_above_lm <- lm(formula = SQM ~ Moon_Magnitude + above_below*Category, data = all_moon_data)
summary(category_above_lm)



# Call:
#   lm(formula = SQM ~ Moon_Magnitude + above_below * Category, data = all_moon_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.1212 -0.3914  0.1033  0.5685  2.3110 
# 
# Coefficients: (7 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              16.77418    0.27514  60.967  < 2e-16 ***
#   Moon_Magnitude                            0.05063    0.02030   2.495  0.01301 *  
#   above_belowBelow                          1.43134    0.13608  10.518  < 2e-16 ***
#   CategoryConnected_Lakes                   1.30681    0.85867   1.522  0.12883    
# CategoryCorn_Lake                         1.10382    0.49541   2.228  0.02643 *  
#   CategoryGMO                               4.46179    0.24115  18.502  < 2e-16 ***
#   CategoryGrand_Mesa                        3.39123    0.60442   5.611 3.79e-08 ***
#   CategoryHighland_Lake                     3.52799    0.32811  10.752  < 2e-16 ***
#   CategoryMoab                              3.70205    0.23904  15.487  < 2e-16 ***
#   CategoryMonument                          2.68879    0.15929  16.879  < 2e-16 ***
#   CategoryRimrock                          -1.52711    0.20069  -7.609 2.02e-13 ***
#   CategoryRock_Slide                       -0.80211    0.25200  -3.183  0.00157 ** 
#   above_belowBelow:CategoryConnected_Lakes       NA         NA      NA       NA    
# above_belowBelow:CategoryCorn_Lake             NA         NA      NA       NA    
# above_belowBelow:CategoryGMO             -1.01821    0.29435  -3.459  0.00060 ***
#   above_belowBelow:CategoryGrand_Mesa            NA         NA      NA       NA    
# above_belowBelow:CategoryHighland_Lake         NA         NA      NA       NA    
# above_belowBelow:CategoryMoab                  NA         NA      NA       NA    
# above_belowBelow:CategoryMonument              NA         NA      NA       NA    
# above_belowBelow:CategoryRimrock               NA         NA      NA       NA    
# above_belowBelow:CategoryRock_Slide      -0.91725    0.32479  -2.824  0.00498 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8503 on 397 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.8261,	Adjusted R-squared:  0.8204 
# F-statistic: 145.1 on 13 and 397 DF,  p-value: < 2.2e-16


# Testing for Moon_Altitude * Moon_Magnitude:

category_above_lm <- lm(formula = SQM ~ (Moon_Magnitude*Moon_Altitude) + (above_below*Category), data = all_moon_data)
summary(category_above_lm)
anova(category_above_lm)

# Call:
#   lm(formula = SQM ~ (Moon_Magnitude * Moon_Altitude) + (above_below * 
#                                                            Category), data = all_moon_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2407 -0.3498  0.0731  0.5489  2.1681 
# 
# Coefficients: (7 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              18.542825   0.634862  29.208  < 2e-16 ***
#   Moon_Magnitude                            0.164553   0.042124   3.906 0.000110 ***
#   Moon_Altitude                             0.039699   0.012971   3.061 0.002359 ** 
#   above_belowBelow                          0.820312   0.238321   3.442 0.000639 ***
#   CategoryConnected_Lakes                   1.736325   0.861933   2.014 0.044639 *  
#   CategoryCorn_Lake                         1.266559   0.493475   2.567 0.010637 *  
#   CategoryGMO                               3.934302   0.293418  13.409  < 2e-16 ***
#   CategoryGrand_Mesa                        3.490293   0.599517   5.822 1.21e-08 ***
#   CategoryHighland_Lake                     3.670618   0.328221  11.183  < 2e-16 ***
#   CategoryMoab                              3.875029   0.243290  15.928  < 2e-16 ***
#   CategoryMonument                          2.823530   0.163626  17.256  < 2e-16 ***
#   CategoryRimrock                          -1.463935   0.199898  -7.323 1.37e-12 ***
#   CategoryRock_Slide                       -0.987417   0.256642  -3.847 0.000139 ***
#   Moon_Magnitude:Moon_Altitude              0.003763   0.001226   3.070 0.002288 ** 
#   above_belowBelow:CategoryConnected_Lakes        NA         NA      NA       NA    
# above_belowBelow:CategoryCorn_Lake              NA         NA      NA       NA    
# above_belowBelow:CategoryGMO             -0.390909   0.354858  -1.102 0.271309    
# above_belowBelow:CategoryGrand_Mesa             NA         NA      NA       NA    
# above_belowBelow:CategoryHighland_Lake          NA         NA      NA       NA    
# above_belowBelow:CategoryMoab                   NA         NA      NA       NA    
# above_belowBelow:CategoryMonument               NA         NA      NA       NA    
# above_belowBelow:CategoryRimrock                NA         NA      NA       NA    
# above_belowBelow:CategoryRock_Slide      -0.557533   0.341954  -1.630 0.103807    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8422 on 395 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.8303,	Adjusted R-squared:  0.8238 
# F-statistic: 128.8 on 15 and 395 DF,  p-value: < 2.2e-16




# > anova(category_above_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq  F value    Pr(>F)    
# Moon_Magnitude                 1  312.55 312.552 440.6852 < 2.2e-16 ***
#   Moon_Altitude                  1    2.57   2.568   3.6215   0.05777 .  
# above_below                    1   32.85  32.845  46.3108 3.758e-11 ***
#   Category                       9 1003.58 111.509 157.2223 < 2.2e-16 ***
#   Moon_Magnitude:Moon_Altitude   1   16.75  16.753  23.6216 1.695e-06 ***
#   above_below:Category           2    2.15   1.076   1.5173   0.22058    
# Residuals                    395  280.15   0.709                       
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
# -3.2193 -0.3976  0.0892  0.7075  1.9883 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              18.89925    0.25234  74.897  < 2e-16 ***
#   Moon_Magnitude                            0.15018    0.02347   6.398 4.50e-10 ***
#   CategoryConnected_Lakes                   1.08294    0.97371   1.112   0.2667    
# CategoryCorn_Lake                         2.26809    1.78850   1.268   0.2055    
# CategoryGMO                               2.93937    0.62481   4.704 3.54e-06 ***
#   CategoryGrand_Mesa                       58.95241 1269.32907   0.046   0.9630    
# CategoryHighland_Lake                     2.03705    1.62701   1.252   0.2113    
# CategoryMoab                              8.88923   22.37083   0.397   0.6913    
# CategoryMonument                          3.38994    0.82609   4.104 4.95e-05 ***
#   CategoryRimrock                          -4.55460    2.27533  -2.002   0.0460 *  
#   CategoryRock_Slide                       -2.32082    1.26603  -1.833   0.0675 .  
# Moon_Magnitude:CategoryConnected_Lakes         NA         NA      NA       NA    
# Moon_Magnitude:CategoryCorn_Lake          0.11887    0.19902   0.597   0.5507    
# Moon_Magnitude:CategoryGMO               -0.06168    0.06594  -0.935   0.3502    
# Moon_Magnitude:CategoryGrand_Mesa         5.93315  136.12106   0.044   0.9653    
# Moon_Magnitude:CategoryHighland_Lake     -0.19251    0.18407  -1.046   0.2963    
# Moon_Magnitude:CategoryMoab               0.66012    2.88948   0.228   0.8194    
# Moon_Magnitude:CategoryMonument           0.05756    0.08975   0.641   0.5217    
# Moon_Magnitude:CategoryRimrock           -0.31699    0.21104  -1.502   0.1339    
# Moon_Magnitude:CategoryRock_Slide        -0.07167    0.11290  -0.635   0.5259    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9625 on 392 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:   0.78,	Adjusted R-squared:  0.7699 
# F-statistic:  77.2 on 18 and 392 DF,  p-value: < 2.2e-16




 
# > anova(light_pollution_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df Sum Sq Mean Sq  F value Pr(>F)    
# Moon_Magnitude            1 312.55 312.552 337.3663 <2e-16 ***
#   Category                  9 969.85 107.761 116.3167 <2e-16 ***
#   Moon_Magnitude:Category   8   5.03   0.629   0.6784 0.7106    
# Residuals               392 363.17   0.926                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lm_location_clean <- lm(SQM ~ Moon_Magnitude + Category, data = all_moon_data)
summary(lm_location_clean)

# Call:
#   lm(formula = SQM ~ Moon_Magnitude + Category, data = all_moon_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.1466 -0.4652  0.1114  0.6755  2.0582 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             18.79230    0.22194  84.671  < 2e-16 ***
#   Moon_Magnitude           0.13993    0.02047   6.837 3.04e-11 ***
#   CategoryConnected_Lakes  1.14151    0.96841   1.179   0.2392    
# CategoryCorn_Lake        1.27667    0.55859   2.286   0.0228 *  
#   CategoryGMO              3.50772    0.15955  21.985  < 2e-16 ***
#   CategoryGrand_Mesa       3.63716    0.68147   5.337 1.58e-07 ***
#   CategoryHighland_Lake    3.70701    0.36958  10.030  < 2e-16 ***
#   CategoryMoab             3.80657    0.26912  14.144  < 2e-16 ***
#   CategoryMonument         2.89485    0.17812  16.252  < 2e-16 ***
#   CategoryRimrock         -1.15544    0.22293  -5.183 3.47e-07 ***
#   CategoryRock_Slide      -1.53018    0.17814  -8.590  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9594 on 400 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.7769,	Adjusted R-squared:  0.7714 
# F-statistic: 139.3 on 10 and 400 DF,  p-value: < 2.2e-16



 lm_location_with_cat <- lm(SQM ~ Moon_Magnitude + Category * Location, data = all_moon_data)
 summary(lm_location_with_cat)
 anova(lm_location_with_cat)
 
 
#  Call:
#    lm(formula = SQM ~ Moon_Magnitude + Category * Location, data = all_moon_data)
#  
#  Residuals:
#    Min      1Q  Median      3Q     Max 
#  -2.4757 -0.2943  0.0000  0.5050  1.8557 
#  
#  Coefficients: (684 not defined because of singularities)
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                                                      16.48476    0.27182  60.646  < 2e-16 ***
#    Moon_Magnitude                                                    0.13340    0.01758   7.588 3.27e-13 ***
#    CategoryConnected_Lakes                                           3.41821    0.81648   4.187 3.63e-05 ***
#    CategoryCorn_Lake                                                 3.13590    0.80971   3.873 0.000129 ***
#    CategoryGMO                                                       5.71562    0.40400  14.148  < 2e-16 ***
#    CategoryGrand_Mesa                                                5.88375    0.58964   9.979  < 2e-16 ***
#    CategoryHighland_Lake                                             5.79314    0.59162   9.792  < 2e-16 ***
#    CategoryMoab                                                      6.06352    0.29211  20.758  < 2e-16 ***
#    CategoryMonument                                                  5.79036    0.80986   7.150 5.51e-12 ***
#    CategoryRimrock                                                   1.31104    0.80980   1.619 0.106400    
#  CategoryRock_Slide                                                2.60296    0.49499   5.259 2.60e-07 ***
#    Location1st_Parking_Lot                                           0.58906    0.96351   0.611 0.541369    
#  Location2nd_Parking_Lot                                                NA         NA      NA       NA    
#  LocationAcross_Array                                              0.31810    0.65725   0.484 0.628706    
#  LocationAcross_Dome                                               0.09523    0.49681   0.192 0.848102    
#  LocationAmerica_Mattress_Outlet                                   1.36083    1.11091   1.225 0.221447    
#  LocationApartment                                                -2.08820    0.47617  -4.385 1.55e-05 ***
#    LocationApartment_Stop-Sign                                            NA         NA      NA       NA    
#  LocationArtists_Point                                            -0.62000    0.87855  -0.706 0.480861    
#  LocationAspen_Main_Path                                           2.52322    0.28750   8.776  < 2e-16 ***
#    LocationAsteria                                                   2.47620    0.37669   6.574 1.89e-10 ***
#    LocationBehind_Hobby_Lobby                                        1.45217    1.11091   1.307 0.192047    
#  LocationBetween_Sprouts-Hobby_Lobby                              -0.31936    1.11155  -0.287 0.774051    
#  LocationBoat_Safety                                               0.01892    0.78553   0.024 0.980801    
#  LocationBrakes_Plus                                              -0.91233    1.11091  -0.821 0.412089    
#  LocationBunting_Parking_Lot                                       2.77855    0.23817  11.666  < 2e-16 ***
#    LocationCampground_Dump_Station                                   0.38056    0.96287   0.395 0.692923    
#  LocationCar                                                       0.09536    0.46018   0.207 0.835961    
#  LocationClamshell                                                -0.02111    0.43037  -0.049 0.960902    
#  LocationCoke_Ovens                                               -0.47968    0.90705  -0.529 0.597275    
#  LocationCold_Shivers                                             -1.94312    1.11098  -1.749 0.081206 .  
#  LocationConfluence-Hotel-Refuge                                   2.59960    0.28750   9.042  < 2e-16 ***
#    LocationDistant_View                                             -0.80139    0.96210  -0.833 0.405459    
#  LocationDollar_Tree_Parking_Lot                                   0.95397    1.11155   0.858 0.391381    
#  LocationDome                                                      0.08791    0.52737   0.167 0.867713    
#  LocationEinstein's                                               -0.52150    1.11091  -0.469 0.639065    
# LocationEinstein_Parking_Lot                                      1.40064    1.11155   1.260 0.208524    
# LocationEntrance                                                  0.03157    0.49681   0.064 0.949374    
# LocationFallen_Rock                                              -0.37251    0.90706  -0.411 0.681569    
# LocationFine-Arts_Bunting_Path                                    3.14037    0.80981   3.878 0.000127 ***
# LocationFoster_Field_House_Sign                                   2.43947    0.58937   4.139 4.42e-05 ***
# LocationFruita_Canyon_View                                       -1.67579    1.11098  -1.508 0.132403    
# LocationGarfield-Escalante_Enterance                              2.25987    0.28238   8.003 2.02e-14 ***
# LocationGrand Mesa-Fine_Arts_Path                                 2.65811    0.29341   9.059  < 2e-16 ***
# LocationGrand_Valley_Overlook                                    -0.27208    1.11110  -0.245 0.806708    
# LocationGrand_View                                               -0.71471    0.87854  -0.814 0.416500    
# LocationHighland_View                                            -0.55025    0.87855  -0.626 0.531538    
# LocationHill_Top                                                  0.34022    0.96287   0.353 0.724055    
# LocationHobby_Lobby_Front                                        -0.26533    1.11091  -0.239 0.811373    
# LocationHouston_Statue                                            2.51115    0.27773   9.042  < 2e-16 ***
# LocationIn_array                                                 -0.38331    0.86475  -0.443 0.657867    
# LocationInspection_Station                                        0.39889    0.96287   0.414 0.678940    
# LocationLiberty_Cap                                              -0.65397    1.11409  -0.587 0.557602    
# LocationLibrary-Grand_Mesa_Field                                  2.47493    0.27362   9.045  < 2e-16 ***
# LocationLowe's_Parking_Lot                                       -1.25520    1.11155  -1.129 0.259612    
#  LocationLowes                                                    -0.40417    1.11091  -0.364 0.716225    
#  LocationMaverick_State                                            2.65859    0.29337   9.062  < 2e-16 ***
#    LocationMaverick_Statue                                           0.52467    0.81011   0.648 0.517648    
#  LocationMound_w_Trees                                             0.02180    0.49681   0.044 0.965024    
#  LocationOtto's_Trail                                             -0.87680    0.90733  -0.966 0.334568    
# LocationOutside_Canyonlands                                            NA         NA      NA       NA    
# LocationParking_Lot                                                    NA         NA      NA       NA    
# LocationPlaza                                                     1.82025    0.30011   6.065 3.56e-09 ***
# LocationQuad                                                      2.56546    0.27361   9.376  < 2e-16 ***
# LocationQudoba_Parking_Lot                                        1.38314    1.11155   1.244 0.214252    
# LocationRandom_Turnoff                                           -0.07271    0.96214  -0.076 0.939808    
# LocationRec_Center_Enterance_Parking_Lot                          1.59814    0.80987   1.973 0.049281 *  
# LocationRed_Canyon                                               -0.26358    1.11113  -0.237 0.812633    
# LocationRed_Robbin                                               -1.44233    1.11091  -1.298 0.195067    
# LocationRimrock_Walmart_Parking_Lot                               0.03614    1.11156   0.033 0.974085    
# LocationRimrock_Walmart_Propagne                                 -0.85770    1.11155  -0.772 0.440885    
# LocationSprouts                                                  -1.44367    1.11091  -1.300 0.194655    
# LocationTelephone_Pole                                                 NA         NA      NA       NA    
# LocationTurnoff                                                        NA         NA      NA       NA    
# LocationUC_Fountain                                               1.95972    0.27773   7.056 9.92e-12 ***
# LocationUpper_Ute_Canyon_View                                    -0.75781    1.11417  -0.680 0.496880    
# LocationUPS_Store                                                -0.20150    1.11091  -0.181 0.856177    
# LocationUte_Canyon                                               -0.96131    0.96209  -0.999 0.318426    
# LocationVisitor_Center                                                 NA         NA      NA       NA    
# LocationWalmart_Greenhouse                                       -3.17750    1.11091  -2.860 0.004499 ** 
# LocationWalmart_Grocery                                          -0.18408    0.96207  -0.191 0.848376    
# LocationWalmart_Home                                                   NA         NA      NA       NA    
# LocationWubben-Theatre_Path                                       2.48382    0.27773   8.943  < 2e-16 ***
# LocationWubben_Courtyard                                          2.24180    0.31713   7.069 9.16e-12 ***
# LocationWubben_Fine_Arts_Field                                    1.57472    0.26991   5.834 1.28e-08 ***
# LocationWubben_Garden_Walk                                             NA         NA      NA       NA    
# CategoryConnected_Lakes:Location1st_Parking_Lot                        NA         NA      NA       NA    
# CategoryCorn_Lake:Location1st_Parking_Lot                              NA         NA      NA       NA    
# CategoryGMO:Location1st_Parking_Lot                                    NA         NA      NA       NA    
# CategoryGrand_Mesa:Location1st_Parking_Lot                             NA         NA      NA       NA    
# CategoryHighland_Lake:Location1st_Parking_Lot                          NA         NA      NA       NA    
# CategoryMoab:Location1st_Parking_Lot                                   NA         NA      NA       NA    
# CategoryMonument:Location1st_Parking_Lot                               NA         NA      NA       NA    
# CategoryRimrock:Location1st_Parking_Lot                                NA         NA      NA       NA    
# CategoryRock_Slide:Location1st_Parking_Lot                             NA         NA      NA       NA    
# CategoryConnected_Lakes:Location2nd_Parking_Lot                        NA         NA      NA       NA    
# CategoryCorn_Lake:Location2nd_Parking_Lot                              NA         NA      NA       NA    
# CategoryGMO:Location2nd_Parking_Lot                                    NA         NA      NA       NA    
# CategoryGrand_Mesa:Location2nd_Parking_Lot                             NA         NA      NA       NA    
# CategoryHighland_Lake:Location2nd_Parking_Lot                          NA         NA      NA       NA    
# CategoryMoab:Location2nd_Parking_Lot                                   NA         NA      NA       NA    
# CategoryMonument:Location2nd_Parking_Lot                               NA         NA      NA       NA    
# CategoryRimrock:Location2nd_Parking_Lot                                NA         NA      NA       NA    
# CategoryRock_Slide:Location2nd_Parking_Lot                             NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationAcross_Array                           NA         NA      NA       NA    
# CategoryCorn_Lake:LocationAcross_Array                                 NA         NA      NA       NA    
# CategoryGMO:LocationAcross_Array                                       NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationAcross_Array                                NA         NA      NA       NA    
# CategoryHighland_Lake:LocationAcross_Array                             NA         NA      NA       NA    
# CategoryMoab:LocationAcross_Array                                      NA         NA      NA       NA    
# CategoryMonument:LocationAcross_Array                                  NA         NA      NA       NA    
# CategoryRimrock:LocationAcross_Array                                   NA         NA      NA       NA    
# CategoryRock_Slide:LocationAcross_Array                                NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationAcross_Dome                            NA         NA      NA       NA    
# CategoryCorn_Lake:LocationAcross_Dome                                  NA         NA      NA       NA    
# CategoryGMO:LocationAcross_Dome                                        NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationAcross_Dome                                 NA         NA      NA       NA    
# CategoryHighland_Lake:LocationAcross_Dome                              NA         NA      NA       NA    
# CategoryMoab:LocationAcross_Dome                                       NA         NA      NA       NA    
# CategoryMonument:LocationAcross_Dome                                   NA         NA      NA       NA    
# CategoryRimrock:LocationAcross_Dome                                    NA         NA      NA       NA    
# CategoryRock_Slide:LocationAcross_Dome                                 NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationAmerica_Mattress_Outlet                NA         NA      NA       NA    
# CategoryCorn_Lake:LocationAmerica_Mattress_Outlet                      NA         NA      NA       NA    
# CategoryGMO:LocationAmerica_Mattress_Outlet                            NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationAmerica_Mattress_Outlet                     NA         NA      NA       NA    
# CategoryHighland_Lake:LocationAmerica_Mattress_Outlet                  NA         NA      NA       NA    
# CategoryMoab:LocationAmerica_Mattress_Outlet                           NA         NA      NA       NA    
# CategoryMonument:LocationAmerica_Mattress_Outlet                       NA         NA      NA       NA    
# CategoryRimrock:LocationAmerica_Mattress_Outlet                        NA         NA      NA       NA    
# CategoryRock_Slide:LocationAmerica_Mattress_Outlet                     NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationApartment                              NA         NA      NA       NA    
# CategoryCorn_Lake:LocationApartment                                    NA         NA      NA       NA    
# CategoryGMO:LocationApartment                                          NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationApartment                                   NA         NA      NA       NA    
# CategoryHighland_Lake:LocationApartment                                NA         NA      NA       NA    
# CategoryMoab:LocationApartment                                         NA         NA      NA       NA    
# CategoryMonument:LocationApartment                                     NA         NA      NA       NA    
# CategoryRimrock:LocationApartment                                      NA         NA      NA       NA    
# CategoryRock_Slide:LocationApartment                                   NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationApartment_Stop-Sign                    NA         NA      NA       NA    
# CategoryCorn_Lake:LocationApartment_Stop-Sign                          NA         NA      NA       NA    
# CategoryGMO:LocationApartment_Stop-Sign                                NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationApartment_Stop-Sign                         NA         NA      NA       NA    
# CategoryHighland_Lake:LocationApartment_Stop-Sign                      NA         NA      NA       NA    
# CategoryMoab:LocationApartment_Stop-Sign                               NA         NA      NA       NA    
# CategoryMonument:LocationApartment_Stop-Sign                           NA         NA      NA       NA    
# CategoryRimrock:LocationApartment_Stop-Sign                            NA         NA      NA       NA    
# CategoryRock_Slide:LocationApartment_Stop-Sign                         NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationArtists_Point                          NA         NA      NA       NA    
# CategoryCorn_Lake:LocationArtists_Point                                NA         NA      NA       NA    
# CategoryGMO:LocationArtists_Point                                      NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationArtists_Point                               NA         NA      NA       NA    
# CategoryHighland_Lake:LocationArtists_Point                            NA         NA      NA       NA    
# CategoryMoab:LocationArtists_Point                                     NA         NA      NA       NA    
# CategoryMonument:LocationArtists_Point                                 NA         NA      NA       NA    
# CategoryRimrock:LocationArtists_Point                                  NA         NA      NA       NA    
# CategoryRock_Slide:LocationArtists_Point                               NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationAspen_Main_Path                        NA         NA      NA       NA    
# CategoryCorn_Lake:LocationAspen_Main_Path                              NA         NA      NA       NA    
# CategoryGMO:LocationAspen_Main_Path                                    NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationAspen_Main_Path                             NA         NA      NA       NA    
# CategoryHighland_Lake:LocationAspen_Main_Path                          NA         NA      NA       NA    
# CategoryMoab:LocationAspen_Main_Path                                   NA         NA      NA       NA    
# CategoryMonument:LocationAspen_Main_Path                               NA         NA      NA       NA    
# CategoryRimrock:LocationAspen_Main_Path                                NA         NA      NA       NA    
# CategoryRock_Slide:LocationAspen_Main_Path                             NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationAsteria                                NA         NA      NA       NA    
# CategoryCorn_Lake:LocationAsteria                                      NA         NA      NA       NA    
# CategoryGMO:LocationAsteria                                            NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationAsteria                                     NA         NA      NA       NA    
# CategoryHighland_Lake:LocationAsteria                                  NA         NA      NA       NA    
# CategoryMoab:LocationAsteria                                           NA         NA      NA       NA    
# CategoryMonument:LocationAsteria                                       NA         NA      NA       NA    
# CategoryRimrock:LocationAsteria                                        NA         NA      NA       NA    
# CategoryRock_Slide:LocationAsteria                                     NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationBehind_Hobby_Lobby                     NA         NA      NA       NA    
# CategoryCorn_Lake:LocationBehind_Hobby_Lobby                           NA         NA      NA       NA    
# CategoryGMO:LocationBehind_Hobby_Lobby                                 NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationBehind_Hobby_Lobby                          NA         NA      NA       NA    
# CategoryHighland_Lake:LocationBehind_Hobby_Lobby                       NA         NA      NA       NA    
# CategoryMoab:LocationBehind_Hobby_Lobby                                NA         NA      NA       NA    
# CategoryMonument:LocationBehind_Hobby_Lobby                            NA         NA      NA       NA    
# CategoryRimrock:LocationBehind_Hobby_Lobby                             NA         NA      NA       NA    
# CategoryRock_Slide:LocationBehind_Hobby_Lobby                          NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationBetween_Sprouts-Hobby_Lobby            NA         NA      NA       NA    
# CategoryCorn_Lake:LocationBetween_Sprouts-Hobby_Lobby                  NA         NA      NA       NA    
# CategoryGMO:LocationBetween_Sprouts-Hobby_Lobby                        NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationBetween_Sprouts-Hobby_Lobby                 NA         NA      NA       NA    
# CategoryHighland_Lake:LocationBetween_Sprouts-Hobby_Lobby              NA         NA      NA       NA    
# CategoryMoab:LocationBetween_Sprouts-Hobby_Lobby                       NA         NA      NA       NA    
# CategoryMonument:LocationBetween_Sprouts-Hobby_Lobby                   NA         NA      NA       NA    
# CategoryRimrock:LocationBetween_Sprouts-Hobby_Lobby                    NA         NA      NA       NA    
# CategoryRock_Slide:LocationBetween_Sprouts-Hobby_Lobby                 NA         NA      NA       NA    
# CategoryConnected_Lakes:LocationBoat_Safety                            NA         NA      NA       NA    
# CategoryCorn_Lake:LocationBoat_Safety                                  NA         NA      NA       NA    
# CategoryGMO:LocationBoat_Safety                                        NA         NA      NA       NA    
# CategoryGrand_Mesa:LocationBoat_Safety                                 NA         NA      NA       NA    
# CategoryHighland_Lake:LocationBoat_Safety                              NA         NA      NA       NA    
# CategoryMoab:LocationBoat_Safety                                       NA         NA      NA       NA    
#  [ reached 'max' / getOption("max.print") -- omitted 561 rows ]
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7855 on 334 degrees of freedom
#   (2 observations deleted due to missingness)
# Multiple R-squared:  0.8751,	Adjusted R-squared:  0.8467 
# F-statistic:  30.8 on 76 and 334 DF,  p-value: < 2.2e-16
 
 
 
 
 # >  anova(lm_location_with_cat)
 # Analysis of Variance Table
 # 
 # Response: SQM
 # Df Sum Sq Mean Sq  F value    Pr(>F)    
 # Moon_Magnitude   1 312.55 312.552 506.5218 < 2.2e-16 ***
 #   Category         9 969.85 107.761 174.6378 < 2.2e-16 ***
 #   Location        66 162.10   2.456   3.9803 < 2.2e-16 ***
 #   Residuals      334 206.10   0.617                       
 # ---
 #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


 
 
 # We control for the moon so we isolate purely the location's light pollution
 baseline_lm <- lm(SQM ~ Moon_Magnitude + above_below + is_baseline, data = comparison_data)
 summary(baseline_lm)
 
 
 # Call:
 #   lm(formula = SQM ~ Moon_Magnitude + above_below + is_baseline, 
 #      data = comparison_data)
 # 
 # Residuals:
 #   Min      1Q  Median      3Q     Max 
 # -3.0227 -0.4384  0.2406  0.6802  2.0481 
 # 
 # Coefficients:
 #   Estimate Std. Error t value Pr(>|t|)    
 # (Intercept)      17.15896    0.29328  58.508  < 2e-16 ***
 #   Moon_Magnitude    0.06164    0.02203   2.798  0.00544 ** 
 #   above_belowBelow  1.07561    0.13149   8.180 5.72e-15 ***
 #   is_baselineTRUE   3.25112    0.11620  27.978  < 2e-16 ***
 #   ---
 #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 # 
 # Residual standard error: 0.9137 on 340 degrees of freedom
 # (2 observations deleted due to missingness)
 # Multiple R-squared:  0.7561,	Adjusted R-squared:  0.7539 
 # F-statistic: 351.2 on 3 and 340 DF,  p-value: < 2.2e-16


 


# ==========================================
# 4.2 Same Day
# ========================================

same_day_lm <- lm(SQM ~ Moon_Magnitude + Category, data = consecutive_sqm)
summary(same_day_lm)
anova(same_day_lm)

# Call:
#   lm(formula = SQM ~ Moon_Magnitude + Category, data = consecutive_sqm)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.1658 -0.2370  0.3738  0.5456  0.8940 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        18.00013    0.19960  90.183  < 2e-16 ***
#   Moon_Magnitude      0.01014    0.02418   0.419 0.675662    
# CategoryGMO         3.04018    0.18557  16.383  < 2e-16 ***
#   CategoryRimrock    -1.61856    0.43926  -3.685 0.000317 ***
#   CategoryRock_Slide -2.01094    0.31268  -6.431 1.51e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8561 on 154 degrees of freedom
# (32 observations deleted due to missingness)
# Multiple R-squared:  0.723,	Adjusted R-squared:  0.7158 
# F-statistic: 100.5 on 4 and 154 DF,  p-value: < 2.2e-16



# > anova(same_day_lm)
# Analysis of Variance Table
# 
# Response: SQM
# Df  Sum Sq Mean Sq  F value  Pr(>F)    
# Moon_Magnitude   1   3.378   3.378   4.6088 0.03337 *  
#   Category         3 291.241  97.080 132.4527 < 2e-16 ***
#   Residuals      154 112.873   0.733                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


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


























