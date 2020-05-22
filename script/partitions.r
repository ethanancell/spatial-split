# -------------------------
# --- SCRIPT PARAMETERS ---
# -------------------------

# Set to true to use the modified loss function in rpart
# to spatially cluster observations more
use_modified_anova <- TRUE
predictors <- c("elev", "lat", "long", "wind_speed", "p1day", "p2day",
                "p3day", "p4day", "p5day", "slope", "aspect",
                "avg_month_rain", "avg_month_temp")

# -----------------
# --- LIBRARIES ---
# -----------------

library(ggplot2)
library(gstat)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rpart)
library(rpart.plot)
library(sf)
library(tidyverse)
 
theme_set(theme_bw())



# --------------------
# ------- DATA -------
# --------------------

# Load the soil measurement stations
soil_data <- read_csv("data/average_september_train.csv")
locations <- soil_data %>%
  dplyr::select(lat, long, sm_8) %>%
  dplyr::filter(sm_8 >= 0)

# We want a dataframe that is "original" but has taken out all the rows
# that were taken out in the modeling step.
post_soil_data <- filter(soil_data, sm_8 >= 0)

# Create dataframe appropriate for modeling
soil_model_df <- soil_data %>%
  # Select appropriate variables
  dplyr::select(sm_8, all_of(predictors)) %>%
  # Take out rows with missing sm_8 (coded as -999999.0)
  dplyr::filter(sm_8 >= 0)



# ---------------------
# ----- PRE-MODEL -----
# ---------------------

# Load the custom splitting, initialization, and evaulation functions from the
# rpart_mod.r script
if (use_modified_anova) {
  source("script/rpart_mod.r")
}



# --------------------
# ----- MODELING -----
# --------------------
# source("script/cluster.r")
# Number of terminal nodes to classify into (subregions)
subregions <- 8
# temp <- cluster(soil_model_df, 'sm_8', 8, "tree_modified")

# TODO: in the model down here, I have latitude and longitude inside of the model. There is a chance
# That you might want to take it out...

# Base our model off of the soil_model_df
if (use_modified_anova) {
  # Get the parameters dataframe with the longitude/latitude of all observations
  parms <- soil_model_df %>%
    select(long, lat)
  disjoint_model <- rpart(sm_8 ~ . -elev, data=soil_model_df, method=anova_mod, parms=parms,cp=0)
} else {
  disjoint_model <- rpart(sm_8 ~ . -long -lat, data=soil_model_df, method="anova", cp=0)
}

# Find the CP we need for the given number of subregions
cptable <- disjoint_model$cptable
needed_cp <- cptable[cptable[,2] == subregions-1, 1]
pruned <- prune(disjoint_model, cp = needed_cp)

# Plot tree
rpart.plot(pruned, type=1)

# Augment our predictions with the nodes 
raw_predictions <- predict(pruned)
predictions <- as.factor(raw_predictions)
# Relabel factors with shorter output
factor_amount <- length(levels(predictions))
levels(predictions) <- 1:factor_amount
soil_model_df <- cbind(soil_model_df, raw_predictions, predictions)

# Find the convex hull for each of the classifications
# ---------
# locs is a list, with each index item being a dataframe with latitude and longitude
# points for all soil moisture stations in the same class
locs <- vector("list", factor_amount)
convex_hulls <- vector("list", factor_amount)
for (flevel in 1:factor_amount) {
  # First copy down all the locations in that class
  locs[[flevel]] <- soil_model_df %>%
    dplyr::select(long, lat) %>%
    dplyr::filter(predictions == flevel)
  # Fill out the convex hull
  convex_hull_index <- chull(as.matrix(locs[[flevel]]))
  convex_hulls[[flevel]] <- locs[[flevel]][convex_hull_index,]
}



# --------------------
# ----- PLOTTING -----
# --------------------

# Country data
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot=FALSE, fill=TRUE))

# Rough boundaries of Utah
xlimit <- c(-114.44, -108.37)
ylimit <- c(36.44, 42.19)

# Colors for the plotted regions
class_colors <- sample(colors(), subregions, replace = TRUE)

# Plot soil moisture stations by "disjoint" classification
classes <- ggplot(data = world) +
  geom_sf() +
  geom_point(data=soil_model_df, aes(x=long, y=lat, color=predictions), size=2, shape=16) +
  coord_sf(xlim=xlimit, ylim=ylimit, expand=FALSE)
#Add on convex polygons
for (flevel in 1:factor_amount) {
  classes <- classes + geom_polygon(data=convex_hulls[[flevel]], 
                                    aes(x = long, y = lat),
                                    alpha = 1/2,
                                    fill = class_colors[flevel])
}
classes <- classes +
  geom_sf(data=states, fill=NA) +
  coord_sf(xlim=xlimit, ylim=ylimit, expand=FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Subregion classifications")

classes



# ALTERNATIVE PLOTTING - JUST SM_8
ggplot(data = world) +
  geom_sf() +
  geom_point(data = soil, aes(x = long, y = lat, color = sm_8), size = 2, shape = 16) +
  geom_sf(data = states, fill = NA) +
  coord_sf(xlim = xlimit, ylim = ylimit, expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Soil moisture")


# --------------------
# ------ OUTPUT ------
# --------------------

# Attach the predicted class to the post modeling dataset
post_soil_data <- cbind(post_soil_data, raw_predictions, predictions)

# Rename columns
post_soil_data <- post_soil_data %>%
  select(sm_2, sm_4, sm_8, sm_20, sm_40,
         st_2, st_4, st_8, st_20, st_40,
         serial_number = "Serial Number", station_name = "Station Name",
         station_id = "Station Id", network = "Network",
         elevation = "Elevation(meter)", lat = "Latitude",
         long = "Longitude", wind_speed = "Wind Speed(m/s)",
         air_temp = "Air Temperature(C)", start_date = "Start Date",
         p1day = "Precipitation for 1 day", p2day = "Precipitation for 2 days",
         p3day = "Precipitation for 3 days", p4day = "Precipitation for 4 days",
         p5day = "Precipitation for 5 days", raw_predictions, predictions)

# Output to a csv for further exploration
write.csv(post_soil_data, "analysis/post_classification.csv")



# -----------------------
# - FURTHER EXPLORATION -
# -----------------------

# What do the distributions of sm_8 look like in each of the regions?
partitioned_data <- vector("list", factor_amount)
for (flevel in 1:factor_amount) {
  partitioned_data[[flevel]] <- post_soil_data %>%
    filter(predictions == flevel)
}

# View the histogram of the partitioned soil region
view_dist_sm <- function(region, bw=0.01, plotnormal=FALSE) {
  region_df <- partitioned_data[[region]]
  n_obs <- dim(region_df)[1]
  plot <- ggplot(region_df) +
    geom_histogram(mapping = aes(x = sm_8), binwidth=bw, color="black")
  
  # Optionally plot the normal curve on top
  if (plotnormal == TRUE) {
    plot <- plot +
      stat_function(fun = function(x) dnorm(
        x,
        mean=mean(region_df$sm_8),
        sd=sd(region_df$sm_8)*bw*n_obs
      ))
  }
  
  plot
}
