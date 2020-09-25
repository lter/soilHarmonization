library(tidyverse)

### locationQC ###
#########################
setwd('C:/github/RCsoilHarmonization/data')

load("locationQC.rda")

locationQC <- locationQC %>% add_row(minValue = 0, 
                                     maxValue = 10,
                                     Unit = "mm",
                                     Var_long = "Coarse Fraction Size Threshold Used",
                                     var = "coarse_tot",
                                     Level = "location",
                                     givenUnit = "mm",
                                     class = "numeric",
                                     definition = NA,
                                     authority = NA
)
  
save(locationQC, file = "locationQC.rda")


# Check if changes are saved
#load("locationQC.rda")
#tail(locationQC, 3)

