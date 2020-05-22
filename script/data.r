require(tidyverse)

load_soil_data <- function(path) {
  unprocessed <- read_csv(path)
  
  # Take out all the observations that have no moisture measurements at sm_8
  processed <- filter(unprocessed, sm_8 >= 0)
  
  processed
}