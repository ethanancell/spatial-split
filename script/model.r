# This function will split up the data into subregions
model_spatial_split <- function(response, predictors, data, subregions, default_rpart = TRUE) {
  require(rpart)
  require(tidyselect)
  require(tidyverse)
  
  # Trim the data to just include the response and the predictors
  data <- data %>%
    dplyr::select(response, all_of(predictors))
  
  # Extract the parameters passed in to create a formula
  formula <- as.formula(paste(response, paste(predictors, collapse = " + "), sep = " ~ "))
  
  if (default_rpart) {
    model <- rpart(formula, data = data, method = "anova", cp = 0)
  }
  
  # Prune the tree correctly
  cptable <- model$cptable
  while (!((subregions - 1) %in% cptable[,2])) {
    subregions <- subregions - 1
  }
  model <- prune(model, cp = cptable[cptable[,2] == subregions - 1, 1])
  
  rpart.plot(model)
  stop()
  browser()
  
  # Return a dataframe with both the raw predictions, as well as the factors of the classes
  prediction <- predict(model)
  soil_class <- as.factor(prediction)
  factor_amount <- length(levels(soil_class)) # Change the factors to something more readable
  levels(soil_class) <- 1:factor_amount
  
  resid <- data %>%
    pull(response)
  resid <- resid - prediction
  
  data.frame(prediction, resid, soil_class)
}


# SPODT oblique spatial partitioning
model_spodt <- function(response, data) {
  require(SPODT)
  require(sp)
  
  # Project into appropriate CRS
  coordinates(data) <- ~long+lat
  proj4string(data) <- crs("+proj=utm +zone=12 +units=m")
  
  clusters <- spodt(sm_8 ~ 1, data = data)
}


# This is the spatial splitting tree that was implemented by 
model_spatstat <- function() {
  require
}