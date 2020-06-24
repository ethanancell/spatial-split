require(gstat)
require(sp)
require(raster)

view_variogram <- function(response, data, new_crs = "+proj=utm +zone=12 +units=km") {
  
  # Transform to spatial object
  coordinates(data) = ~long+lat
  proj4string(data) <- crs("+proj=longlat +datum=WGS84")
  data <- spTransform(data, new_crs)
  
  vgm <- gstat::variogram(as.formula(paste(response, "~ 1")), data = data)
  plot(vgm)
}

# Don't autorun
if (FALSE) {
  # SOIL
  maybe <- a_soil_st
  maybe <- maybe %>%
    mutate(sq_sm8 = sqrt(sm_8))
  
  soilclass <- maybe %>%
    filter(soil_class == 5)
  coordinates(soilclass) <- ~long+lat
  proj4string(soilclass) <- crs("+proj=longlat +datum=WGS84")
  soilclass <- spTransform(soilclass, "+proj=utm +zone=12, +units=km")
  v <- gstat::variogram(sq_sm8 ~ p1day, data = soilclass)
  plot(v)
  
  # Try to fit a variogram
  v.fit = fit.variogram(v, vgm("Sph"))
}

# Don't autorun this section
if (FALSE) {
  # Look at the soil classes
  #maybe <- snow
  #maybe <- read_csv("maybe_maybe_snow.csv")
  maybe <- a_soil
  maybe$soil_class <- as.factor(maybe$soil_class)
  
  # Add the log transform of yr50
  maybe <- maybe %>%
    mutate(l_yr50 = log(yr50))
  
  # Plot the distributions of l_yr50 by the predicted class
  par(mfrow = c(2, 3))
  for (i in 1:4) {
    hist(dplyr::filter(maybe, soil_class == i)$sm_8, 
         main = paste("Snow class ", i),
         xlab = "Log yr50")
  }
  
  # These plots might be formed from the "lattice" package, so there might be another
  # way to plot multiple of these onto a grid than how you do it with base R.
  # Plot the variograms for each of them
  dev.off()
  par(mfrow = c(2, 3))
  for (i in 1:5) {
    snowclass <- maybe %>%
      filter(soil_class == i)
    coordinates(snowclass) <- ~long+lat
    proj4string(snowclass) <- crs("+proj=longlat +datum=WGS84")
    # Might want to adjust the cutoff in the variogram
    snowclass <- spTransform(snowclass, "+proj=utm +zone=12, +units=km")
    vgm <- gstat::variogram(l_yr50 ~ ELEVATION, data = snowclass, cutoff)
    plot(vgm)
  }
  
  # View variogram for all the data
  snowcopy <- maybe
  coordinates(snowcopy) <- ~long+lat
  proj4string(snowcopy) <- crs("+proj=longlat +datum=WGS84")
  snowcopy <- spTransform(snowcopy, "+proj=utm +zone=12 +units=km")
  vgm <- gstat::variogram(l_yr50 ~ ELEVATION, data = snowcopy)
  dev.off()
  par(mfrow = c(1,1))
  plot(vgm)
  
  # Prove there is a spatial element
  g1 <- gstat::gstat(NULL, "test", formula = log(yr50) ~ ELEVATION, locations = myclass)
  g_var <- gstat::fit.variogram(gstat::variogram(g1), gstat::vgm("Sph"))
  plot(gstat::variogram(g1), g_var)
}