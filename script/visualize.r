require(maps)
require(rnaturalearth)
require(rnaturalearthdata)
require(sf)
require(tidyverse)

theme_set(theme_bw())

# This function will return a map of Utah that ggplot can plot on top of
plot_utah <- function() {
  world <- ne_countries(scale = "medium", returnclass = "sf")
  states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  
  xlimit <- c(-114.44, -108.37)
  ylimit <- c(36.44, 42.19)
  
  ggplot(data = world) +
    geom_sf() +
    geom_sf(data = states, fill = NA) +
    coord_sf(xlim = xlimit, ylim = ylimit, expand = FALSE)
}


# Draw the soil moisture stations
plot_measurements <- function(plot_var, data) {
  utah <- plot_utah()
  
  utah +
    geom_point(data = data, aes(x = long, y = lat, color = resid), size = 2, shape = 16) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Soil moisture stations")
}


# Plot the predicted classes
plot_soil_classes <- function(data) {
  utah <- plot_utah()
  
  utah + 
    geom_point(data = data, aes(x = long, y = lat, color = soil_class), size = 2, shape = 16) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Soil classes")
}