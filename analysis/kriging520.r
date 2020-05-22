library(gstat)
library(missForest)
library(randomForest)
library(raster)
library(rgdal)
library(sp)
library(tidyverse)

soil <- read_csv("data/average_september_train.csv")
soil <- soil %>%
  filter(sm_8 >= 0)

predictors <- c("elev", "lat", "long", "p1day", "p2day",
                "p3day", "p4day", "p5day", "slope", "aspect",
                "avg_month_rain", "avg_month_temp")
predictor_df <- dplyr::select(soil, all_of(predictors))

# Create imputed and removed dataset
temp <- missForest(cbind(soil$sm_8, predictor_df))
soil_rf_imputed <- temp$ximp
soil_rf_imputed <- dplyr::rename(soil_rf_imputed, sm_8 = 'soil$sm_8')

coordinates(soil_rf_imputed) = ~long+lat
proj4string(soil_rf_imputed) <- crs("+proj=longlat +datum=WGS84")

# Project to UTM Zone 12
soil_utah <- spTransform(soil_rf_imputed, crs("+proj=utm +zone=12 +units=m"))

# plot Utah
ggplot(data = as.data.frame(soil_utah)) +
  geom_point(mapping = aes(x = long, y = lat, color = sm_8))

# Examine anisotropy
utah_gstat <- gstat(id="SM_8", formula = sm_8 ~ 1, data = soil_utah)
utah_vgm <- variogram(utah_gstat, map = TRUE, cutoff = 100000, width = 6000)
plot(utah_vgm, threshold=10)

vgm <- variogram(sm_8 ~ 1, data = soil_rf_imputed)
plot(vgm)

# Fit variogram
vgm_model <- vgm(psill = 0.0049, model = "Gau", nugget = 0.002, range = 0.25)
plot(vgm, model = vgm_model)
vgm.fit <- fit.variogram(vgm, model = vgm(1, "Exp"))

# Examine anisotropy
vgm_gstat <- gstat(id = "SM_8", formula = sm_8 ~ 1, data = soil_rf_imputed)
vgm <- variogram(vgm_gstat, map = TRUE, cutoff = 3, width = 200)
plot(vgm, threshold = 10)
