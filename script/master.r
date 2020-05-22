library(tidyverse)

# Load required functions for the pipeline
source("script/data.r")
source("script/geostat.r")
source("script/model.r")
source("script/visualize.r")

# Import the processed data
data_source <- "data/average_september_train.csv"
soil <- load_soil_data(data_source)

# What will be included in the predictive model?
predictors <- c("elev", "lat", "long", "wind_speed", "p1day", "p2day",
                "p3day", "p4day", "p5day", "slope", "aspect",
                "avg_month_rain", "avg_month_temp")

# Get the result from the pruned tree
clusters <- model_spatial_split("sm_8", predictors, soil, 20)
soil <- cbind(soil, clusters)

# Visualize
plot_measurements("resid", soil)
plot_soil_classes(soil)

# Try "sm_8" or "resid"
view_variogram("resid", soil)
