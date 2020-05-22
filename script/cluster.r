library(tidyverse)

# This file implements a few different methods of spatially clustering data.
cluster <- function(data, response, subregions, method, x='long', y='lat') {
  
  # -----------------------------
  # -- REGULAR REGRESSION TREE --
  # -----------------------------
  if (method == "tree_original") {
    
  }
  
  
  # ------------------------------
  # -- MODIFIED REGRESSION TREE --
  # ------------------------------
  else if (method == "tree_modified") {
    library(rpart)
    
    # Load modified script
    source("script/rpart_mod.r")
    
    parms <- soil_model_df %>%
      select(x, y)
    model <- rpart(response ~ ., data = data, 
                   method = anova_mod, parms = parms,
                   cp = 0)
    
    # Extract from CP table
    cptable <- disjoint_model$cptable
    
    while (!((subregions - 1) %in% cptable[,2])) {
      subregions <- subregions - 1
    }
    
    needed_cp <- cptable[cptable[,2] == subregions-1, 1]
    pruned_model <- prune(model, cp = needed_cp)
    
    # Augment predictions and classes
    raw_predictions <- predict(pruned_model)
    predictions <- as.factor(raw_predictions)
    # Relabel factors with shorter output
    factor_amount <- length(levels(predictions))
    levels(predictions) <- 1:factor_amount
    
    # Return
    cbind(data, raw_predictions, predictions)
  }
  
  
  # ---------------------------
  # -- UNSUPERVISED LEARNING --
  # ---------------------------
  else if (method == "unsupervised") {
    
  }
}