# This is a script to perfect an algorithm that will create spatial "blocks" out of some training data.
# These blocks can be used for a spatial cross-validation algorithm.

library(autocart)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)

soil_orig <- read_csv("data/average_september_train.csv")

# Script necessary to group
source("src/clusterStations.R")
soil <- soil_orig %>%
  mutate(cluster = as.factor(cluster_stations(soil_orig$long, soil_orig$lat, soil_orig$elev,
                                              dist_adj = 100,
                                              elev_adj = 0,
                                              h = 2)))
soil <- soil %>%
  dplyr::select("sm_8", "elev", "p1day", "p2day", "p3day", "p4day", "p5day",
                "long", "lat", "slope", "aspect", "avg_month_rain", "avg_month_temp", "cluster")
soil <- na.omit(soil)

# Plot the data
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
xlimit <- c(-114.44, -108.37)
ylimit <- c(36.44, 42.19)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) +
  coord_sf(xlim = xlimit, ylim = ylimit, expand = FALSE) +
  geom_point(data = soil, aes(x = long, y = lat, color = cluster), size = 2, shape = 16) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Soil moisture stations")

# Cross-validate with the generated chunks
y <- soil$sm_8
x <- soil %>%
  dplyr::select("elev", "p1day", "p2day", "p3day", "p4day", "p5day",
                "long", "lat", "slope", "aspect", "avg_month_rain", "avg_month_temp")
loc <- as.matrix(dplyr::select(soil, "long", "lat"))
autocartModel <- autocart(y, x, loc, 1, 0, autocartControl(minbucket = 8, distpower = 2))

# Print out the groups that autocart generates
soil_view <- cbind(soil, autocartModel$predAsFactor)
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) +
  coord_sf(xlim = xlimit, ylim = ylimit, expand = FALSE) +
  geom_point(data = soil_view, aes(x = long, y = lat, color = autocartModel$predAsFactor), size = 2, shape = 16) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Soil moisture stations")


# Cross-validate
xvs <- soil$cluster
pred <- rep(NA, length = nrow(x))
for (k in 1:length(levels(soil$cluster))) {
  train_x <- x[xvs != k, ]
  train_y <- y[xvs != k]
  train_loc <- loc[xvs != k, ]
  
  test_x <- x[xvs == k, ]
  test_loc <- loc[xvs == k, ]
  
  trainedModel <- autocart(train_y, train_x, train_loc, 0.60, 0.40, autocartControl(distpower=2))
  
  pred[xvs == k] <- predictSpatialNodes(trainedModel, test_x, test_loc, 10)
}

# Cross-validate with rpart
library(rpart)
data <- cbind(y, x)
pred <- rep(NA, length = nrow(x))
for (k in 1:length(levels(soil$cluster))) {
  train <- data[xvs != k, ]
  test <- data[xvs == k, ]
  
  trainedModel <- rpart(y ~ ., data = data)
  
  pred[xvs == k] <- predict(trainedModel, test)
}