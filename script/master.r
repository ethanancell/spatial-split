library(raster)
library(tidyverse)
library(autocart)
library(gstat)
library(sp)

# Load required functions for the pipeline
source("script/data.r")
#source("script/geostat.r")
#source("script/model.r")
source("script/visualize.r")

# Import the processed data
data_source <- "data/average_september_train.csv"
soil <- load_soil_data(data_source)

# What will be included in the predictive model?
predictors <- c("elev", "p1day", "p2day",
                "p3day", "p4day", "p5day", "long", "lat", "slope", "aspect",
                "avg_month_rain", "avg_month_temp")

soil <- dplyr::select(soil, "sm_8", all_of(predictors))
soil <- na.omit(soil)

# Process the dataset to where it can be used by autocart
response <- as.matrix(soil$sm_8)
predictors <- soil %>%
  dplyr::select(all_of(predictors))
locations <- soil %>%
  dplyr::select("long", "lat")
locations <- as.matrix(locations)









# Test creating regions
# ==================
autocart_model <- autocart(response, predictors, locations, .7, .3, 
                           autocartControl(distpower=2))
# Turn the clusters into easy factors
soil_class <- autocart_model$predAsFactor
levels(soil_class) <- 1:length(levels(soil_class))
a_soil <- cbind(soil, soil_class)
plot_soil_classes(a_soil)
# ==================

# Look at variogram for a region
# ==================
class <- 5
soilclass <- a_soil %>%
  dplyr::filter(soil_class == class)
coordinates(soilclass) <- ~long+lat
proj4string(soilclass) <- crs("+proj=longlat +datum=WGS84")
soilclass <- spTransform(soilclass, "+proj=utm +zone=12 +units=km")
v <- gstat::variogram(sm_8 ~ 1, data = soilclass)
plot(v)
# ==================









# Same thing, but with "full stationarity"
full_stationarity <- autocart(response, predictors, locations, 1, 0, autocartControl(distpower=2))
soil_class <- as.factor(full_stationarity$prediction)
levels(soil_class) <- 1:length(levels(soil_class))
a_soil_st <- cbind(soil, soil_class)

# Visualize
plot_measurements("resid", soil)
plot_soil_classes(a_soil_st)

# Try "sm_8" or "resid"
view_variogram("sm_8", soil)









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