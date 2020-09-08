library(tidyverse)

### unitsConversion ###
#########################
setwd('C:/github/RCsoilHarmonization/data')

load("unitsConversions.rda")

new_units <- unitsConversions %>% filter(var == "lyr_c_tot") %>% 
                                  mutate(var = "grav_water_content") %>%
                                  mutate(Var_long = "Gravimetric water content")

unitsConversions <- unitsConversions %>% add_row(new_units)

#To fix mistakes, cut off rows (3 row cut shown here)
#unitsConversions = slice(unitsConversions, 1:(n()-3))

save(unitsConversions, file = "unitsConversions.rda")


# Check if changes are saved
#load("profileQC.rda")
#tail(profileQC, 3)