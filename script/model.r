# This function will split up the data into subregions
model_spatial_split <- function(response, predictors, data, subregions, default_rpart = TRUE) {
  require(rpart)
  require(tidyselect)
  require(tidyverse)
  require(ape)
  
  #browser()
  
  # Extract the longitude/latitude information
  locations <- data %>%
    dplyr::select(long, lat)
  
  # Trim the data to just include the response and the predictors
  data <- data %>%
    dplyr::select(response, all_of(predictors))
  
  # Extract the parameters passed in to create a formula
  formula <- as.formula(paste(response, paste(all_of(predictors), collapse = " + "), sep = " ~ "))
  
  if (default_rpart) {
    model <- rpart(formula, data = data, method = "anova", cp = 0)
  } else {
    # Use the modified model. The "anova_mod" elements come from the helper script.
    source("script/helper/rpart_mod.r")
    anova_mod <- list(eval = eval_mod, split = split_mod, init = init_mod)
    
    # rpart allows you to change the splitting criteria by passing in all the modded functions
    # in the form of a list (anova_mod).
    model <- rpart(formula, data = data, method = anova_mod, parms = locations, cp = 0)
  }
  
  # Prune the tree to have the desired number of subregions
  cptable <- model$cptable
  while (!((subregions - 1) %in% cptable[,2])) {
    subregions <- subregions - 1
  }
  model <- prune(model, cp = cptable[cptable[,2] == subregions - 1, 1])
  
  #rpart.plot(model)
  
  # Return a dataframe with both the raw predictions, as well as the clusters
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
