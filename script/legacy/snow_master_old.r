library(tidyverse)
library(autocart)

# Load required functions for the pipeline
source("script/data.r")
source("script/geostat.r")
source("script/model.r")
source("script/visualize.r")

# Import the processed data
snow <- read.csv("data/alternate_data/ut2017_snow_final.csv")
snow <- snow %>%
  rename(long = "LONGITUDE", lat = "LATITUDE", elev = "ELEVATION")

# Dataframe of predictors
predictors <- snow %>%
  dplyr::select("long", "lat", "elev", "YRS", "maxobs", "HUC", "TD", "FFP", "MCMT",
         "MWMT", "PPTWT", "RH", "MAT")
response <- snow$yr50
locations <- cbind(snow$long, snow$lat)

# Fill in the missing values
for (i in 1:ncol(predictors)) {
  predictors[is.na(predictors[, i]), i] <- mean(predictors[, i], na.rm = TRUE)
}

# Take a subset of the data, as memory issues lead autocart to crash with >415 observations.
r_sample <- sample(1:length(response), 250)
response_sub <- response[r_sample]
predictors_sub <- predictors[r_sample, ]
locations_sub <- locations[r_sample, ]

# Get the result from autocart
# clusters <- model_spatial_split("yr50", predictors, snow, 15, default_rpart = FALSE)
#cluster <- autocart(response, predictors, locations, 0.90)
soil_class <- autocart(response, predictors, locations, 0.85)

# Turn clusters into a factor and append it to the snow dataframe
soil_class <- as.factor(soil_class)
levels(soil_class) <- 1:length(levels(soil_class))
snow <- cbind(snow, soil_class)

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