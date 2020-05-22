require(gstat)
require(sp)
require(raster)

view_variogram <- function(response, data, crs = "+proj=utm +zone=12 +units=m") {
  
  # Transform to spatial object
  coordinates(data) = ~long+lat
  proj4string(data) <- crs(crs)
  
  vgm <- variogram(as.formula(paste(response, "~ 1")), data = data)
  plot(vgm)
}