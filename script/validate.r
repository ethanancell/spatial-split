require(tidyverse)
require(fields)

#      Metrics
# -------------------
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

# --------------------------------
# -- Inverse Distance Weighting --
# --------------------------------

# Regular 10-fold cross validation
xvs <- rep(1:10, length = nrow(soil))
xvs <- sample(xvs)
pred <- rep(NA, length = nrow(soil))
for (k in 1:10) {
  train <- soil[xvs != k, ]
  test <- soil[xvs == k, ]
  
  # Transform everything to the right space
  coordinates(train) <- ~long+lat
  proj4string(train) <- crs("+proj=longlat +datum=WGS84")
  train <- spTransform(train, CRS("+proj=utm +zone=12 +units=m"))
  coordinates(test) <- ~long+lat
  proj4string(test) <- crs("+proj=longlat +datum=WGS84")
  test <- spTransform(test, CRS("+proj=utm +zone=12 +units=m"))
    
  # Create the IDW model from the training set and then extract to the test locations
  train.raster <- as(raster(train, res=1000), "SpatialPixels")
  train.idw <- idw(sm_8~1, train, train.raster)
  pred[xvs == k] <- raster::extract(raster(train.idw), test, method = "simple")
}

# Metrics
rmae(pred, soil$sm_8)
rmse(pred, soil$sm_8)

plot_data <- dplyr::select(soil, long, lat)

# -------------------------------------
# -- IDW with clustered observations --
# -------- in the Meyer spirit --------
# -------------------------------------

source("script/helper/cluster_stations.r")
max_station_distance <- max(fields::rdist.earth(matrix(c(soil$long, soil$lat), ncol=2), miles = FALSE))
max_elevation_diff <- max(soil$elev) - min(soil$elev)
validate_clusters <- cluster_stations(soil$long, soil$lat, soil$elev,
                                      max_station_distance, max_elevation_diff, 0.7)

pred <- rep(NA, length = nrow(soil))
for (k in 1:10) {
  train <- soil[validate_clusters != k, ]
  test <- soil[validate_clusters == k, ]
  
  # Transform everything to the right space
  coordinates(train) <- ~long+lat
  proj4string(train) <- crs("+proj=longlat +datum=WGS84")
  train <- spTransform(train, CRS("+proj=utm +zone=12 +units=m"))
  coordinates(test) <- ~long+lat
  proj4string(test) <- crs("+proj=longlat +datum=WGS84")
  test <- spTransform(test, CRS("+proj=utm +zone=12 +units=m"))
  
  # Create the IDW model from the training set and then extract to the test locations
  train.raster <- as(raster(train, res=1000), "SpatialPixels")
  train.idw <- idw(sm_8~1, train, train.raster)
  pred[validate_clusters == k] <- raster::extract(raster(train.idw), test, method = "simple")
}

rmae(pred, soil$sm_8)
rmse(pred, soil$sm_8)
