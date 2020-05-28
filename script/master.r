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
predictors <- c("elev", "wind_speed", "p1day", "p2day",
                "p3day", "p4day", "p5day", "slope", "aspect",
                "avg_month_rain", "avg_month_temp")

# Get the result from the pruned tree
clusters <- model_spatial_split("sm_8", predictors, soil, 20)
soil <- cbind(soil, clusters)

# Visualize
plot_measurements("resid", soil)
plot_soil_classes(soil)

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