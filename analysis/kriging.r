library(gstat)
library(sp)
library(tidyverse)

# --------------
# ---- DATA ----
# --------------

soil <- read.csv("analysis/post_classification.csv")

# Convert back to factor
soil$predictions <- as.factor(soil$predictions)

# Add transforms
soil <- soil %>%
  mutate(sqrt(sm_2), sqrt(sm_4), sqrt(sm_8), sqrt(sm_20)) %>%
  rename(sqrt_sm_2 = `sqrt(sm_2)`, sqrt_sm_4 = `sqrt(sm_4)`,
         sqrt_sm_8 = `sqrt(sm_8)`, sqrt_sm_20 = `sqrt(sm_20)`)

# Add residuals column
soil <- soil %>%
  mutate(resid = raw_predictions - sm_8)

# Extract by class
factor_amount <- length(levels(soil$predictions))
partitioned_data <- vector("list", factor_amount)
for (flevel in 1:factor_amount) {
  partitioned_data[[flevel]] <- soil %>%
    filter(predictions == flevel)
}

# ---------------------------
# - EXPLORATION / TRANSFORM -
# ---------------------------

# Helper function to view the distribution of a variable at an
# enumerated region. 
view_dist <- function(region, var, bw=0.01, plotnormal=FALSE) {
  region_df <- partitioned_data[[region]]
  n_obs <- dim(region_df)[1]
  plot <- ggplot(region_df) +
    geom_histogram(mapping = aes(x = var), binwidth=bw, color="black")
  
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

# Explore class3
class3explore <- partitioned_data[[7]]
coordinates(class3explore) = ~long+lat

allclasses <- rbind(partitioned_data[[1:12]])
coordinates(soil) = ~long+lat

# Variogram
vgm <- variogram(resid~1, data=soil)
plot(vgm)
vgm.fit <- fit.variogram(vgm, model = vgm(1, "Sph", 900, 1))

# ---------------
# ---- MODEL ----
# ---------------
