# This script is for a new experimental design that will be more universal.

# This script defines the first step to a more accurate modeling method. The result of this will be a model that can be applied anywhere on Earth and hopefully a published study!


# ==========================================
# VIIRS RADIANCE EXTRACTION SCRIPT
# ==========================================
# Install if necessary: install.packages("terra")
library(tidyverse)
library(terra)

# 1. Load the VIIRS Monthly Composite GeoTIFF
# Replace with your specific extracted filename
viirs_raster <- rast("SVDNB_npp_20260301-20260331_75N180W_vcmcfg_v10_c202604071200.avg_rade9h.tif")

# 2. Define the exact centroid of your light dome 
# (e.g., coordinates for the CMU Campus / Grand Junction core)
dome_centroid <- data.frame(
  Location = "Grand_Junction_Core",
  lon = -108.5540,  
  lat = 39.0805
)

# Convert coordinates to a spatial vector matching the satellite's projection (WGS84)
city_point <- vect(dome_centroid, geom = c("lon", "lat"), crs = "EPSG:4326")

# 3. Extract the Radiance (I_0)
# We use a small buffer (e.g., 500 meters) and take the mean to avoid single-pixel anomalies
I_0_data <- extract(viirs_raster, city_point, buffer = 500, fun = mean)

print("Baseline Urban Radiance (I_0):")
print(I_0_data)












# ==========================================
# TOPOGRAPHIC SHIELDING (VIEWSHED) SCRIPT
# ==========================================
library(terra)
library(elevatr)
library(sf)

# 1. Define your city centroid (Grand Junction Core)
centroid_lon <- -108.5540
centroid_lat <- 39.0805

# 2. Define the geographic bounds of your experiment
bounds <- data.frame(
  lon = c(-109.0, -108.2), 
  lat = c(38.9, 39.3)      
)
bounds_sf <- st_as_sf(bounds, coords = c("lon", "lat"), crs = 4326)

# 3. Download the Digital Elevation Model (DEM)
dem_raster <- get_elev_raster(locations = bounds_sf, z = 10, clip = "bbox")
dem_lonlat <- rast(dem_raster)

# ---------------------------------------------------------
# NEW STEP: PROJECT TO METERS (UTM Zone 12N)
# ---------------------------------------------------------
# Project the DEM from degrees into meters
dem_proj <- project(dem_lonlat, "EPSG:32612")

# We must also project your centroid point into the exact same meter system
centroid_point <- vect(data.frame(lon = centroid_lon, lat = centroid_lat), geom = c("lon", "lat"), crs = "EPSG:4326")
centroid_proj <- project(centroid_point, "EPSG:32612")

# Extract the new X/Y coordinates (now in meters instead of lon/lat)
loc_meters <- crds(centroid_proj)[1, ]
# ---------------------------------------------------------


# 4. Calculate the Viewshed
dome_viewshed <- viewshed(
  dem_proj, 
  loc = loc_meters, 
  observer = 500,  # 500 meters ABOVE the ground level at the centroid
  target = 1.5     # 1.5 meters above the ground (your SQM holding height)
)

# 5. Plot the result
plot(dome_viewshed, main = "Light Dome Visibility (1 = Visible, 0 = Shielded)", col = c("black", "yellow"))
points(loc_meters[1], loc_meters[2], col = "red", pch = 19, cex = 1.5) # Mark the city core




# ==========================================
# EMPTY DISTANCE-DECAY MODEL (PRE-FLIGHT TEST)
# ==========================================
# Install if necessary: install.packages(c("geosphere", "minpack.lm"))
library(geosphere)
library(minpack.lm)

# 1. Dummy Data (Simulating your future drive away from the city)
transect_data <- data.frame(
  Stop = 1:5,
  lat = c(39.10, 39.15, 39.20, 39.25, 39.30),
  lon = c(-108.60, -108.65, -108.70, -108.75, -108.80),
  SQM = c(18.5, 19.8, 20.5, 21.2, 21.6)
)

# 2. Calculate Haversine Distance (in meters, then convert to km)
# geosphere requires coordinates in c(longitude, latitude) order
city_centroid <- c(-108.5540, 39.0805) 

transect_data$distance_km <- distHaversine(
  p1 = matrix(c(transect_data$lon, transect_data$lat), ncol = 2),
  p2 = city_centroid
) / 1000

# 3. Convert logarithmic SQM to linear irradiance (I)
# SQM is a magnitude scale. To do physical math, we must make it linear.
transect_data$Irradiance <- 10^(-0.4 * transect_data$SQM)

# 4. Define our known Source Constant
I_0 <- 64.36  # Baseline Urban Radiance extracted from VIIRS

# 5. The Non-Linear Least Squares Model
# Formula: Irradiance = (I_0 / distance^2) * e^(-alpha * distance) + I_base
decay_model <- nlsLM(
  Irradiance ~ (I_0 / (distance_km^2)) * exp(-alpha * distance_km) + I_base,
  data = transect_data,
  start = list(
    alpha = 0.1,         # Initial guess for atmospheric attenuation
    I_base = 0.00001     # Initial guess for natural dark sky irradiance
  ),
  control = nls.lm.control(maxiter = 100)
)

# View the mathematical results
print(summary(decay_model))

# 6. Plot the Curve!
# Let's visualize the light dome collapsing as you drive away
plot(transect_data$distance_km, transect_data$Irradiance,
     main = "Grand Junction Light Dome Decay Model",
     xlab = "Distance from Centroid (km)",
     ylab = "Linear Sky Irradiance",
     pch = 16, col = "blue", cex = 1.5)

# Generate a smooth line based on the model's solved equation
dist_seq <- seq(min(transect_data$distance_km), max(transect_data$distance_km), length.out = 100)
pred_irrad <- predict(decay_model, newdata = list(distance_km = dist_seq))
lines(dist_seq, pred_irrad, col = "red", lwd = 2)
















