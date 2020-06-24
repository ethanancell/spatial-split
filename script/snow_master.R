library(tidyverse)
library(autocart)
library(gstat)
library(raster)
library(sp)

# Load required functions for the pipeline
source("script/visualize.r")

# Import the processed data
snow <- read.csv("data/alternate_data/ut2017_snow_final.csv")
snow <- snow %>%
  rename(long = "LONGITUDE", lat = "LATITUDE", elev = "ELEVATION")
snow <- na.omit(snow)

# Dataframe of predictors
predictors <- snow %>%
  dplyr::select("long", "lat", "elev", "YRS", "HUC", "TD", "FFP", "MCMT",
         "MWMT", "PPTWT", "RH", "MAT")
predictors$HUC <- as.factor(predictors$HUC)
response <- snow$yr50
locations <- cbind(snow$long, snow$lat)





# CREATE REGION
# ===============
snow_model <- autocart(response, predictors, locations, 1, 0,
                       autocartControl(distpower = 2, minsplit = 50))
soil_class <- snow_model$predAsFactor
levels(soil_class) <- 1:length(levels(soil_class))
final_snow <- cbind(snow, soil_class)
final_snow <- final_snow %>%
  dplyr::mutate(l_yr50 = log(yr50), meanofclass = snow_model$prediction)
plot_soil_classes(final_snow)
# ===============

boxplot(l_yr50 ~ soil_class, data=final_snow)

# VIEW VARIOGRAM FOR A CLASS
# ===============
class <- 2
soilclass <- final_snow %>%
  dplyr::filter(soil_class == class)
coordinates(soilclass) <- ~long+lat
proj4string(soilclass) <- crs("+proj=longlat +datum=WGS84")
#soilclass <- spTransform(soilclass, "+proj=utm +zone=12 +units=km")
v <- gstat::variogram(yr50 ~ 1, data = soilclass, cutoff=150, cressie=TRUE)
plot(v)
# ===============


ggplot(data = final_snow) +
  geom_point(aes(x = elev, y = yr50, color=soil_class)) +
  geom_smooth(aes(x = elev, y = yr50, color=soil_class))