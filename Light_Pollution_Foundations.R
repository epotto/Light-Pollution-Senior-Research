# ==============================================================================
# SQM Data Analysis (CMU) - RStudio Edition
# Author: Ethan Otto
# ==============================================================================

# ==============================================================================
# PART 0: R CRASH COURSE (For Python / Mathematica Users)
# ==============================================================================
# Welcome! Since you are used to Python/Mathematica, here are a few R quirks:
# 1. Assignment: R uses `<-` instead of `=` for assigning variables. 
# 2. Help: Type `?` before any function (e.g., `?mean`) to open the help menu.
# 3. Pipes: The `|>` symbol passes data from one step to the next. 
#    It is exactly like method chaining in Python (e.g., df.filter().groupby()).

# Let's play with a built-in toy dataset called 'mtcars' (Motor Trend Car Road Tests)
print("Loading toy dataset...")
toy_data <- mtcars 

# View() opens the dataframe in a nice spreadsheet tab in RStudio (like a Pandas DataFrame)
View(toy_data)

# Here is how we filter and summarize data using the pipe `|>`
# (We need the tidyverse package for this, which we load in Part 1)
# 
# example_summary <- toy_data |>
#   filter(cyl == 4) |>                 # Keep only 4-cylinder cars
#   summarise(avg_mpg = mean(mpg))      # Calculate the average MPG
# 
# print(example_summary)

# ==============================================================================
# PART 1: SETUP AND DATA IMPORT
# ==============================================================================
# tidyverse includes dplyr (for data wrangling) and ggplot2 (for graphing)
library(tidyverse)
library(lubridate)
library(hms)
library(lmtest)

# Set your working directory in RStudio (Session -> Set Working Directory) 
# before running this line to ensure it finds your CSV.
data <- read.csv("Research_Data_Final.csv", stringsAsFactors = FALSE)

# ==============================================================================
# PART 2: FILTERING & CALCULATION FUNCTIONS
# ==============================================================================

manual_filter <- function(data) {
  # Limiting to 29 columns to drop trailing empty/unnecessary Excel columns
  dat_filt <- data[, 1:29] |>
    mutate(
      Date_Time = parse_date_time(paste(Date, Time), orders = c("mdy_HMS", "mdy_HM", "ymd_HMS", "ymd_HM")),
      hour_of_day = hour(Date_Time), 
      Night_of = if_else(hour_of_day < 12, as.Date(Date_Time) - days(1), as.Date(Date_Time))
    ) |>
    filter(!is.na(Date_Time))
  return(dat_filt)
}

manual_calculations <- function(data) {
  dat_filt <- manual_filter(data)
  
  dat_filt |>
    mutate(reading_group = (row_number() - 1) %/% 6) |>
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
}

extract_extra_variables <- function(data) {
  data |>
    mutate(
      Extra_Variables = replace_na(Extra_Variables, ""),
      is_baseline    = str_detect(Extra_Variables, "Baseline"),
      is_car_on      = str_detect(Extra_Variables, "Car_On"),
      true_dark_baseline = if_else(Category %in% c("GMO", "Highline_Lake", "Grand_Mesa", "Monument", "Moab"), TRUE, FALSE)
    )
}

moon_filter <- function(data, site_category = "All") {
  if (site_category != "All") {
    data <- data |> filter(Category == site_category)
  }
  
  data |>
    mutate(
      across(c(Moon_Phase_Pcnt, Moon_Magnitude, SQM, variance, Moon_Alt_Calc), as.numeric),
      above = Moon_Alt_Calc > 0,
      above_below = ifelse(Moon_Alt_Calc > 0, "Above", "Below")
    )
}

# ==============================================================================
# PART 3: DATA PROCESSING EXECUTION
# ==============================================================================

# Applying our functions cleanly using the pipe operator
sqm_mean <- manual_calculations(data) |>
  extract_extra_variables()

# Generate main dataset
all_moon_data <- moon_filter(sqm_mean, "All")

# Subsets for specific testing
clean_moon_data <- subset(all_moon_data, Sun_Alt_Calc <= -18) # True astronomical night
new_moon_data <- sqm_mean |> filter(Moon_Phase_Name %in% c("New Moon", "New_Moon"))

# Isolate consecutive readings for same-day tracking
consecutive_sqm <- clean_moon_data |>
  group_by(Category, Location, Night_of) |>
  filter(n_distinct(Date_Time) > 1) |>
  ungroup() |>
  arrange(Category, Night_of, Date_Time)

# ==============================================================================
# PART 4: CORE STATISTICAL MODELING
# ==============================================================================

# 1. Weighted Non-Linear Least Squares (Lunar Impact)
cat("\n--- WNLS Model: Lunar Impact ---\n")
lunar_wnls <- nls(SQM ~ base_sqm + Above * as.logical(above), 
                  data = clean_moon_data,
                  start = list(base_sqm = 16.59, Above = -1),
                  weights = variance)
print(summary(lunar_wnls))

# 2. Best General Model: Lunar Impact by Location
cat("\n--- Linear Model: Lunar Impact by Location ---\n")
location_lunar_lm <- lm(SQM ~ Moon_Magnitude + Moon_Alt_Calc * Category, data = clean_moon_data)
print(summary(location_lunar_lm))
bptest(location_lunar_lm) # Check homoscedasticity

# 3. Pure Light Dome Effect (New Moon Only)
cat("\n--- Linear Model: Pure Light Dome (No Moon) ---\n")
pure_light_dome_lm <- lm(SQM ~ true_dark_baseline, data = new_moon_data)
print(summary(pure_light_dome_lm))

# ==============================================================================
# PART 5: PRESENTATION VISUALIZATIONS
# ==============================================================================

# Plot 1: SQM vs Moon Brightness
p1 <- ggplot(all_moon_data, aes(x = Moon_Magnitude, y = SQM, color = above_below)) +
  scale_x_reverse() +
  geom_point(alpha = 0.5, size = 2) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  labs(
    title = "Impact of Moon Phase on Sky Brightness",
    x = "Moon Phase (Magnitude)",
    y = expression("SQM (mag/arcsec"^2*")"),
    color = "Moon Position"
  ) +
  theme_minimal(base_size = 14) + 
  theme(legend.position = "top", plot.title = element_text(face = "bold"))
#print(p1)

# Plot 2: SQM Over Time of Night
p2 <- ggplot(data = all_moon_data, aes(x = as_hms(Date_Time), y = SQM)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_smooth(method = "lm", se = FALSE, color = "#F8766D", linewidth = 1.2) +
  scale_x_time(name = "Time of Night", breaks = hms::hms(hours = c(0, 4, 8, 12, 16, 20, 24))) +
  coord_cartesian(ylim = c(14, 22)) + 
  labs(
    title = "SQM Readings Over Time (All Locations)",
    subtitle = "Data grouped by time of acquisition"
  ) +
  theme_minimal()
#print(p2)

# Plot 3: Pure Light Pollution Comparison (New Moon Data)
p3 <- ggplot(new_moon_data, aes(x = reorder(Category, -SQM), y = SQM, fill = true_dark_baseline)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_reverse() + 
  scale_fill_manual(values = c("FALSE" = "#E69F00", "TRUE" = "#56B4E9"),
                    labels = c("Light Polluted (Valley)", "True Dark Sky (Baseline)")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Pure Light Pollution: New Moon Comparison",
    subtitle = "All differences shown are due to location, not the moon",
    x = "Location Category",
    y = expression("SQM (mag/arcsec"^2*")"),
    fill = "Site Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#print(p3)

# To save a plot for your presentation/paper, use:
# ggsave("light_pollution_boxplot.png", plot = p3, width = 8, height = 5)








