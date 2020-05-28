require(gstat)
require(sp)
require(raster)

view_variogram <- function(response, data, crs = "+proj=utm +zone=12 +units=m") {
  
  # Transform to spatial object
  coordinates(data) = ~long+lat
  proj4string(data) <- crs("+proj=longlat +datum=WGS84")
  
  vgm <- variogram(as.formula(paste(response, "~ 1")), data = data)
  plot(vgm)
}




# TEST ZONE
soil.df <- soil
coordinates(soil.df) <- ~long+lat
proj4string(soil.df) <- crs("+proj=longlat +datum=WGS84")
soil.df <- spTransform(soil.df, CRS("+proj=utm +zone=12 +units=m"))

# make a raster
soil.raster <- as(raster(soil.df, res=1000), "SpatialPixelsDataFrame")

# idw
soil.idw <- idw(sm_8 ~ elev, soil.df, soil.raster)

# Cross-validate

