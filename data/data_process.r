# ---------------
# -- LIBRARIES --
# ---------------

library(raster)
library(rasterVis)
library(tidyverse)

# -----------------
# - STATION LEVEL -
# -----------------

example_data <- read.csv("data/example_data.csv")
locations <- example_data %>%
  select(long = "Longitude", lat = "Latitude")

# Convert to sp form
coordinates(locations) <- ~long+lat

# ---------------
# ----- DEM -----
# ---------------

utah_dem <- raster("data/dem/ut_raster_c.gri")
area_slope <- terrain(utah_dem, opt = 'slope', unit = 'degrees')
area_aspect <- terrain(utah_dem, opt = 'aspect', unit = 'degrees')

# Overlay
aspect_vec <- raster::extract(area_aspect, locations, method = 'simple')
slope_vec <- raster::extract(area_slope, locations, method = 'simple')

example_data <- cbind(example_data, slope_vec, aspect_vec)
write.csv(example_data, file = "data/test_output.csv")

# --------------
# -- PLOTTING --
# --------------

rasterVis::levelplot(utah_dem,
                     margin = list(x = FALSE, y = TRUE),
                     col.regions = terrain.colors(16),
                     xlab = list(label = '', vjust = -0.25),
                     sub = list(
                       label = "masl",
                       font = 1,
                       cex = 0.9,
                       hjust = 1.5))
