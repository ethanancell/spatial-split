library(ggplot2)
library(maps)
library(randomForest)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)

# ------------
# --- DATA ---
# ------------

example_soil <- read.csv("data/test_output.csv")

# What will we include in the model?
predictors <- predictors <- c(elev = "Elevation.meter.", 
                              wind = "Wind.Speed.m.s.",
                              p1day = "Precipitation.for.1.day", 
                              p2day = "Precipitation.for.2.days",
                              p3day = "Precipitation.for.3.days", 
                              p4day = "Precipitation.for.4.days",
                              p5day = "Precipitation.for.5.days", 
                              slope = "slope_vec",
                              aspect = "aspect_vec", 
                              long = "Longitude",
                              lat = "Latitude")

# Rename columns and remove observations with nothing at sm_8
example_soil <- example_soil %>%
  select(sm_8, all_of(predictors)) %>%
  filter(sm_8 >= 0)


# -------------
# --- MODEL ---
# -------------

example_rf <- randomForest(sm_8 ~ . -long -lat, 
                           data = example_soil, 
                           na.action = "na.roughfix",
                           importance = TRUE)

prediction <- predict(example_rf)
example_soil <- cbind(example_soil, prediction)
example_soil <- example_soil %>%
  mutate(resid = sm_8 - prediction)

# ----------------
# --- ANALYSIS ---
# ----------------

# Cross-validated predictions
cv_predictions <- rep(0, nrow(example_soil))
xvs <- rep(1:10, length = nrow(example_soil))
xvs <- sample(xvs)
for (i in 1:10) {
  train <- example_soil[xvs != i, ]
  test <- example_soil[xvs == i, ]
  
  partial_model <- randomForest(sm_8 ~ . -long -lat -prediction -resid,
                                data = train,
                                na.action = "na.roughfix")
  cv_predictions[xvs==i] <- predict(partial_model, test)
}

cv_resid <- example_soil$sm_8 - cv_predictions
hist(cv_resid)

# Plot
plot_data <- cbind(example_soil, cv_predictions, cv_resid)

# World data
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot=FALSE, fill=TRUE))

# Rough boundaries of Utah
xlimit <- c(-114.44, -108.37)
ylimit <- c(36.44, 42.19)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = plot_data, mapping = aes(x = long, y = lat, color = cv_resid),
             size = 2, shape = 16) +
  geom_sf(data = states, fill = NA) +
  coord_sf(xlim = xlimit, ylim = ylimit, expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("RF Residuals")

# ----------------
# ---- OUTPUT ----
# ----------------
write.csv(plot_data, file = "data/output/rf_residuals.csv")
