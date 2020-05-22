library(ggplot2)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)
 
theme_set(theme_bw())

# Country data
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot=FALSE, fill=TRUE))

# Load the soil measurement stations
soil_data <- read_csv("data/example_data.csv")
locations <- soil_data %>%
  select(lat = Latitude, long = Longitude, moisture = sm_8) %>%
  filter(moisture >= 0)

# Draw a polygon around the three sites with:
# * Most west station, most east station, and most north station
s1 <- filter(locations, long == min(long))
s2 <- filter(locations, long == max(long))
s3 <- filter(locations, lat == max(lat))
polygon_outer_triangle_df <- rbind(s1, s2, s3)
# Polygon of convex hull
hull_locations <- chull(as.matrix(locations[,1:2]))
polygon_hull <- locations[hull_locations, 1:2]

# Rough boundaries of Utah
xlimit <- c(-114.44, -108.37)
ylimit <- c(36.44, 42.19)

# Plot convex hull of all data for a single day
ggplot(data=world) +
  geom_sf() + 
  geom_point(data=locations, aes(x=long, y=lat, color=moisture), size=2, shape=16) +
  #geom_polygon(data = polygon_hull, aes(x = long, y = lat), alpha = 1/3, fill = "green") +
  geom_sf(data=states, fill=NA) +
  coord_sf(xlim=xlimit, ylim=ylimit, expand=FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Soil moisture stations")

# Randomly split the data into two, form convex hulls around those
p1_sample <- sample(1:dim(locations)[1], size = dim(locations)[1] %/% 2, replace = FALSE)
p1 <- locations[p1_sample,]
p2 <- locations[-p1_sample,]
p1_hull <- chull(as.matrix(p1[,1:2]))
p2_hull <- chull(as.matrix(p2[,1:2]))
# Locations of the convex hull around randomly sampled data
p1_loc <- p1[p1_hull, 1:2]
p2_loc <- p2[p2_hull, 1:2]

# Plot two convex hulls from random subsets of the data
ggplot(data = world) +
  geom_sf() +
  # Moisture stations
  geom_point(data=locations, aes(x=long, y=lat), color="red", size=1, shape=16) +
  # Hulls
  geom_polygon(data = p1_loc, aes(x = long, y = lat), alpha = 1/4, fill = "blue") +
  geom_polygon(data = p2_loc, aes(x = long, y = lat), alpha = 1/4, fill = "yellow") +
  # Rest of map
  geom_sf(data=states, fill=NA) +
  coord_sf(xlim=xlimit, ylim=ylimit, expand=FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Soil moisture stations")


# ---------------------------------
# ------- VIGNETTE TUTORIALS ------
# ---------------------------------
nc <- st_read(system.file("shape/nc.shp", package="sf"))
nc_geom <- st_geometry(nc)
