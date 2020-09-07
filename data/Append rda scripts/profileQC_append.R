library(tidyverse)

### profileQC ###
#########################
setwd('C:/github/RCsoilHarmonization/data')

load("profileQC.rda")
colnames(profileQC)
profileQC <- profileQC %>% add_row(minValue = 0, 
                                     maxValue = 100,
                                     unit_levels = NA,
                                     Var_long = "This is a RC test var",
                                     var = "test",
                                     Level = "profile",
                                     givenUnit = "g cm-3",
                                     class = "numeric",
                                     type = "value",
                                     hardunit = "g cm-3",
                                     definition = NA,
                                     authority = NA
)
  
save(profileQC, file = "profileQC.rda")


# Check if changes are saved
#load("profileQC.rda")
#tail(profileQC, 3)

