library(autocart)
library(tidyverse)

# Load required functions for the pipeline
source("script/data.r")
source("script/geostat.r")
source("script/model.r")
source("script/visualize.r")


# Import the processed data
snow <- read.csv("data/alternate_data/ut2017_snow_final.csv")

# ===========================
# ======== FUNCTIONS ========
# ===========================

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

# ==========================
# ======== AUTOCART ========
# ==========================

# Process dataset to make it suitable for autocart
ac_response <- as.matrix(snow$yr50)
ac_snow <- data.frame(snow$LONGITUDE, snow$LATITUDE, snow$ELEVATION, snow$YRS, snow$HUC,
                      snow$TD, snow$FFP, snow$MCMT, snow$MWMT, snow$PPTWT, snow$RH, snow$MAT)
ac_locations <- as.matrix(cbind(snow$LONGITUDE, snow$LATITUDE))

# Give all missing values the average of non-missing column values
for (i in 1:ncol(ac_snow)) {
  ac_snow[is.na(ac_snow[, i]), i] <- mean(ac_snow[, i], na.rm = TRUE)
}

# 10-fold cross-validation with alpha = 0.85
alpha <- 0.85
xvs <- rep(1:10, length = nrow(ac_snow))
xvs <- sample(xvs)
ac_pred <- rep(NA, length = nrow(ac_snow))
for (k in 1:10) {
  train <- ac_snow[xvs != k, ]
  test <- ac_snow[xvs == k, ]
  
  train_y <- ac_response[xvs != k]
  test_y <- ac_response[xvs == k]
  
  train_loc <- ac_locations[xvs != k, ]
  test_loc <- ac_locations[xvs == k, ]
  
  # Create the model based off the test dataset
  trained_model <- autocart(train_y, train, train_loc, alpha)
  
  ac_pred[xvs == k] <- predictAutocart(trained_model, test)
}

# Results
test_predictions(ac_pred, ac_response)


# =========================
# ========- RPART =========
# =========================

library(rpart)

# Process dataset to make it suitable for autocart
rp_snow <- data.frame(snow$yr50, snow$LONGITUDE, snow$LATITUDE, snow$ELEVATION, snow$YRS, snow$HUC,
                      snow$TD, snow$FFP, snow$MCMT, snow$MWMT, snow$PPTWT, snow$RH, snow$MAT)

# Give all missing values the average of non-missing column values
for (i in 1:ncol(rp_snow)) {
  rp_snow[is.na(rp_snow[, i]), i] <- mean(rp_snow[, i], na.rm = TRUE)
}

# 10-fold cross-validation with alpha = 0.85
xvs <- rep(1:10, length = nrow(rp_snow))
xvs <- sample(xvs)
rp_pred <- rep(NA, length = nrow(rp_snow))
for (k in 1:10) {
  train <- rp_snow[xvs != k, ]
  test <- rp_snow[xvs == k, ]
  
  # Create the model based off the test dataset
  trained_model <- rpart(snow.yr50 ~ ., data=train)
  
  rp_pred[xvs == k] <- predict(trained_model, test)
}

# Results
test_predictions(rp_pred, ac_response)







# Make sure the cluster column is of type "factor"
snow$cluster <- as.factor(snow$cluster)

# Visualize
plot_measurements("resid", snow)
plot_soil_classes(snow)

# Try "sm_8" or "resid"
view_variogram("yr50", snow)









# TEST ZONE

# SPODT partitioning. Trying to figure out how to 
# incorporate this into the model.r script,
# for the time being it will reside right here.
library(SPODT)

soil_copy <- soil
coordinates(soil_copy) <- ~long+lat
proj4string(soil_copy) <- crs("+proj=utm +zone=12")
model <- spodt(sm_8 ~ 1, data = soil_copy)
soil_class <- model@partition
soil_class <- as.factor(soil_class)

lines <- spodtSpatialLines(model, data = soil_copy)
lines <- spTransform(lines, crs("+proj=longlat"))
lines_df <- SpatialLinesDataFrame(lines, data.frame(ID = 1:length(lines@lines)), match.ID = F)

# plot nicely
utah <- plot_utah()
utah +
  geom_path(data = lines_df, mapping = aes(x = long, y = lat, group = group))

soil <- cbind(soil, soil_class)
plot_soil_classes(soil)