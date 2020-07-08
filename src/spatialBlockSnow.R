# This is a script to perfect an algorithm that will create spatial "blocks" out of some training data.
# These blocks can be used for a spatial cross-validation algorithm.

library(autocart)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)


# ============================
# === EVALUATION FUNCTIONS ===
# ============================

# The functions contained here are to evaluate the results given by autocart and rpart

# Relative mean absolute error
rmae <- function(pred, obs, na.rm = TRUE) {
  if (length(pred) != length(obs)) {
    stop("Predicted and observed vectors must be same length.")
  }
  mean(abs((pred-obs)/mean(obs))*100, na.rm = na.rm)
}

# Root mean square error
rmse <- function(pred, obs, na.rm = TRUE) {
  if (length(pred) != length(obs)) {
    stop("Predicted and observed vectors must be same length.")
  }
  sqrt(mean((pred-obs)^2, na.rm = na.rm))
}

# Output both rmse and rmae in one easy paste function
test_predictions <- function(pred, obs, na.rm = TRUE) {
  if (length(pred) != length(obs)) {
    stop("Predicted and observed vectors must be same length.")
  }
  
  print(paste("RMAE is ", rmae(pred, obs, na.rm), ", and RMSE is ", rmse(pred, obs, na.rm), sep=""))
}

# ======================
# ======== DATA ========
# ======================

snow_orig <- read_csv("data/ut2017_snow.csv")

# Script necessary to group
source("src/clusterStations.R")
snow <- snow_orig %>%
  mutate(cluster = as.factor(cluster_stations(snow_orig$LONGITUDE, snow_orig$LATITUDE, snow_orig$ELEVATION,
                                              dist_adj = 30,
                                              elev_adj = 0,
                                              h = 3)))
snow <- snow %>%
  dplyr::select("yr50", "LONGITUDE", "LATITUDE", "ELEVATION", "YRS", "HUC", "TD",
                "FFP", "MCMT", "MWMT", "PPTWT", "RH", "MAT", "cluster")
snow <- na.omit(snow)
snow$yr50 <- log(snow$yr50)

# Plot the data
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
xlimit <- c(-114.44, -108.37)
ylimit <- c(36.44, 42.19)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) +
  coord_sf(xlim = xlimit, ylim = ylimit, expand = FALSE) +
  geom_point(data = snow, aes(x = LONGITUDE, y = LATITUDE, color = cluster), size = 2, shape = 16) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Snow measurement stations")




# How does regular inverse distance weighting do?
# Regress yr50 on elev then do IDW
#snowLM <- glm(yr50 ~ ELEVATION, data = snow)
#snowSpatial <- cbind(actual = predict(snowLM, snow), snow)
snowSpatial <- snow
coordinates(snowSpatial) <- ~LONGITUDE+LATITUDE
proj4string(snowSpatial) <- crs("+proj=longlat +datum=WGS84")
snowSpatial <- spTransform(snowSpatial, crs("+proj=utm +zone=12 +units=km"))
snowRaster <- raster(snowSpatial, res = 8)

gs <- gstat(formula = yr50 ~ 1, data=snowSpatial, set = list(idp = 2))
thisIDW <- interpolate(snowRaster, gs)
plot(thisIDW)

# Cross-validate with IDW
xvs <- snow$cluster
pred <- rep(NA, length = nrow(x))
for (k in 1:length(levels(snow$cluster))) {
  thisData <- snow[xvs != k, ]
  predictData <- snow[xvs == k ,]
  
  coordinates(thisData) <- ~LONGITUDE+LATITUDE
  proj4string(thisData) <- crs("+proj=longlat +datum=WGS84")
  thisData <- spTransform(thisData, crs("+proj=utm +zone=12 +units=km"))
  thisRaster <- raster(thisData, res = 8)
  
  coordinates(predictData) <- ~LONGITUDE+LATITUDE
  proj4string(predictData) <- crs("+proj=longlat +datum=WGS84")
  predictData <- spTransform(predictData, crs("+proj=utm +zone=12 +units=km"))
  
  gs <- gstat(formula = yr50 ~ 1, data = thisData, set = list(idp = 1))
  thisIDW <- interpolate(thisRaster, gs)
  
  pred[xvs == k] <- raster::extract(thisIDW, predictData)
}

test_predictions(pred, snow$yr50)

# Cross-validate with the generated chunks
y <- snow$yr50
x <- snow %>%
  dplyr::select("LONGITUDE", "LATITUDE", "ELEVATION", "YRS", "HUC", "TD",
                "FFP", "MCMT", "MWMT", "PPTWT", "RH", "MAT")
loc <- as.matrix(dplyr::select(snow, "LONGITUDE", "LATITUDE"))

autocartModel <- autocart(y, x, loc, 0.80, 0.10, autocartControl(distpower = 2))

# Print out the groups that autocart generates
snow_view <- cbind(snow, autocartModel$predAsFactor)
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) +
  coord_sf(xlim = xlimit, ylim = ylimit, expand = FALSE) +
  geom_point(data = snow_view, aes(x = LONGITUDE, y = LATITUDE, color = autocartModel$predAsFactor), size = 2, shape = 16) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Snow classes")


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

# ===================================
# ===== RPART + CROSSVALIDATION =====
# ===================================

library(rpart)
data <- cbind(y, x)
pred <- rep(NA, length = nrow(x))
xvs <- snow$cluster
for (k in 1:length(levels(snow$cluster))) {
  train <- data[xvs != k, ]
  test <- data[xvs == k, ]
  
  trainedModel <- rpart(y ~ ., data = train)
  
  pred[xvs == k] <- predict(trainedModel, test)
}
test_predictions(pred, snow$yr50)