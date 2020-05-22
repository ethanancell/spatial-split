# ---------------
# -- LIBRARIES --
# ---------------

library(plyr)
library(dplyr)
library(rasterVis)
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)
library(raster)

# -----------------------
# -- SCRIPT PARAMETERS --
# -----------------------

utah_west <- -114.03
utah_east <- -109
utah_north <- 42
utah_south <- 36.59

# -------------------
# --- AVERAGE MAP ---
# -------------------

# load in all of the soil maps from the daily folder
daily_maps <- list.files("data/daily/", pattern = "csv$", full.names = TRUE)
daily_maps <- lapply(daily_maps, read_csv)

# Replace -999999 with NA
daily_maps <- lapply(daily_maps, na_if, y=-999999)

# Rename columns
daily_maps <- lapply(daily_maps, dplyr::rename, serial_number = "Serial Number", station_name = "Station Name",
                     station_id = "Station Id", elev = "Elevation(meter)", network = "Network",
                     long = "Longitude", lat = "Latitude", wind_speed = "Wind Speed(m/s)",
                     air_temp = "Air Temperature(C)", start_date = "Start Date",
                     p1day = "Precipitation for 1 day", p2day = "Precipitation for 2 days",
                     p3day = "Precipitation for 3 days", p4day = "Precipitation for 4 days",
                     p5day = "Precipitation for 5 days")

# Order by station name
daily_maps <- lapply(daily_maps, dplyr::arrange, station_name)

# Columns we wish to average over
average_columns <- c("wind_speed", "air_temp", "p1day", "p2day", "p3day", "p4day", "p5day",
                     "sm_2", "sm_4", "sm_8", "sm_20", "sm_40", "st_2", "st_4", "st_8", "st_20", "st_40")

# Put the average numbers in a dataframe
average_subset <- lapply(daily_maps, dplyr::select, all_of(average_columns))
non_average_subset <- lapply(daily_maps, dplyr::select, -all_of(average_columns))

# Take out 12, because not all the stations came through
trimmed_average_subset <- average_subset[-12]
test <- Reduce("+", trimmed_average_subset) / length(trimmed_average_subset)

# Bind the non-averaged columns with the average rows
average_df <- cbind(non_average_subset[[1]], test)
write_csv(average_df, "data/average_september_train.csv")

# -----------------
# - STATION LEVEL -
# -----------------

example_data <- daily_maps[[3]]
# example_data <- read.csv("data/example_data.csv")
locations <- example_data %>%
  dplyr::select(long = "Longitude", lat = "Latitude")



# Convert to sp form
coordinates(locations) <- ~long+lat

# Rename columns
example_data <- example_data %>%
  dplyr::rename(serial_number = "Serial Number", station_name = "Station Name",
                station_id = "Station Id", elev = "Elevation(meter)", network = "Network",
                long = "Longitude", lat = "Latitude", wind_speed = "Wind Speed(m/s)",
                air_temp = "Air Temperature(C)", start_date = "Start Date",
                p1day = "Precipitation for 1 day", p2day = "Precipitation for 2 days",
                p3day = "Precipitation for 3 days", p4day = "Precipitation for 4 days",
                p5day = "Precipitation for 5 days")

# Replace -999999 with NA
example_data <- na_if(example_data, -999999)

# Dataframe with averages
average_df <- read_csv("data/average_september_train_preprocess.csv")
average_locations <- average_df %>%
  dplyr::select(long, lat)

# ---------------
# ----- DEM -----
# ---------------

utah_dem <- raster("data/dem/ut_raster_c.gri")
area_slope <- terrain(utah_dem, opt = 'slope', unit = 'degrees')
area_aspect <- terrain(utah_dem, opt = 'aspect', unit = 'degrees')

# Overlay
aspect <- raster::extract(area_aspect, locations, method = 'simple')
slope <- raster::extract(area_slope, locations, method = 'simple')

example_data <- cbind(example_data, slope, aspect)

# Average DF
aspect <- raster::extract(area_aspect, average_locations, method = 'bilinear')
slope <- raster::extract(area_slope, average_locations, method = 'bilinear')
average_df <- cbind(average_df, slope, aspect)

# --------------
# -- RAINFALL --
# --------------

require(raster)
require(rgdal)

rainfall_raster <- raster("data/prism/september_precipitation/PRISM_ppt_30yr_normal_800mM2_09_bil.bil")
avg_month_rain <- raster::extract(rainfall_raster, locations, method = 'simple')
example_data <- cbind(example_data, avg_month_rain)

# Average dataframe
avg_month_rain <- raster::extract(rainfall_raster, average_locations, method = 'bilinear')
average_df <- cbind(average_df, avg_month_rain)

# -----------------
# -- TEMPERATURE --
# -----------------

temperature_raster <- raster("data/prism/september_temperature/PRISM_tmean_30yr_normal_800mM2_09_bil.bil")
avg_month_temp <- raster::extract(temperature_raster, locations, method = 'simple')
example_data <- cbind(example_data, avg_month_temp)

# Average dataframe
avg_month_temp <- raster::extract(temperature_raster, average_locations, method = "bilinear")
average_df <- cbind(average_df, avg_month_temp)

# ----------
# -- SILT --
# ----------

siltlist <- list.files("data/soilgrids/silt", pattern = "tif$", full.names = TRUE)
siltgrids <- lapply(siltlist, raster)
# Merge silt grids together
siltgrid <- siltgrids[[1]]
for (i in 2:length(siltgrids)) {
  siltgrid <- raster::extend(siltgrid, siltgrids[[i]])
}

# Add to layer
silt <- raster::extract(siltgrids[[1]], locations, method = "simple")

# ---------------
# -- ELEVATION --
# ---------------

elevation_raster <- raster("data/prism/elevation/PRISM_us_dem_800m_bil.bil")
elevation <- raster::extract(elevation_raster, locations,)

# ------------------------
# -- PREDICTION DATASET --
# ------------------------

# When extents are different, R will throw an error
find_correct_extent <- function(...) {
  #current_extent <- raster::intersect(extent(first_layer), extent(second_layer))
  #current_extent
  layers <- list(...)
  current_extent <- raster::extent(layers[[1]])
  for (i in 2:length(layers)) {
    current_extent <- raster::intersect(raster::extent(current_extent), raster::extent(layers[[i]]))
  }
  current_extent
  #current_extent <- extent(layers[[1]])
  #for (i in 2:length(layers)) {
  #  current_extent <- intersect(extent(current_extent), extent(layers[[i]]))
  #}
  #current_extent
}

# Crop rasters
bound <- as(extent(utah_west, utah_east, utah_south, utah_north), 'SpatialPolygons')
crs(bound) <- "+proj=longlat"

utah_dem <- crop(utah_dem, bound)
area_slope <- crop(area_slope, bound)
area_aspect <- crop(area_aspect, bound)
rainfall_raster <- crop(rainfall_raster, bound)

# Make the same size
correct_extent <- find_correct_extent(utah_dem, area_slope, area_aspect, rainfall_raster)
utah_dem <- crop(utah_dem, correct_extent)
area_slope <- crop(area_slope, correct_extent)
area_aspect <- crop(area_aspect, correct_extent)
rainfall_raster <- crop(rainfall_raster, correct_extent)

prediction_stack <- stack(utah_dem, area_slope, area_aspect, rainfall_raster)

prediction_brick <- brick(utah_dem, area_slope,
                          area_aspect, rainfall_raster)

prediction_brick <- brick(utah_dem, area_slope, 
                          area_aspect, rainfall_raster, 
                          temperature_raster)

# ----------
# -- SAVE --
# ----------

write_csv(example_data, "data/train.csv")
write_csv(average_df, "data/average_september_train.csv")

# --------------
# -- PLOTTING --
# --------------

# Country data
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot=FALSE, fill=TRUE))

# Rough boundaries of Utah
xlimit <- c(-114.44, -108.37)
ylimit <- c(36.44, 42.19)

# Remove all the observations where sm_8 is less than 0
plot_data <- example_data %>%
  filter(sm_8 >= 0)

# Plot soil moisture stations by "disjoint" classification
classes <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data=states, fill=NA) +
  #geom_sf(data = rain_sf, mapping = aes(color = SIG_WX_TYPE)) +
  geom_point(data = plot_data, mapping = aes(x = long, y = lat, color = sm_8)) +
  coord_sf(xlim=xlimit, ylim=ylimit, expand=FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Polygons")

classes

rasterVis::levelplot(merge(siltgrids[[1]], siltgrids[[2]]),
                     margin = list(x = FALSE, y = TRUE),
                     col.regions = terrain.colors(16),
                     xlab = list(label = '', vjust = -0.25),
                     sub = list(
                       label = "masl",
                       font = 1,
                       cex = 0.9,
                       hjust = 1.5))
