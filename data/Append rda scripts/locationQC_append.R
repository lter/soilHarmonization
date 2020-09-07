library(tidyverse)

### locationQC ###
#########################
setwd('C:/github/RCsoilHarmonization/data')

load("locationQC.rda")

locationQC <- locationQC %>% add_row(minValue = 0, 
                                     maxValue = 100,
                                     Unit = "m",
                                     Var_long = "This is a RC test var",
                                     var = "test",
                                     Level = "location",
                                     givenUnit = "m",
                                     class = "character",
                                     definition = NA,
                                     authority = NA
)
  
save(locationQC, file = "locationQC.rda")


# Check if changes are saved
#load("locationQC.rda")
#tail(locationQC, 3)

