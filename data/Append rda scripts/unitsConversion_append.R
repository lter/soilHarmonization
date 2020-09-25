library(tidyverse)

### unitsConversion ###
#########################
setwd('C:/github/RCsoilHarmonization/data')

load("unitsConversions.rda")

new_units <- unitsConversions %>% filter(var == "bd_tot") %>% 
                                  mutate(var = "bd_tot_mdl") %>%
                                  mutate(Var_long = "Bulk Density modeled, With Coarse Fragments")

unitsConversions <- unitsConversions %>% add_row(new_units)

#To fix mistakes, cut off rows (3 row cut shown here)
#unitsConversions = slice(unitsConversions, 1:(n()-3))

save(unitsConversions, file = "unitsConversions.rda")


# Check if changes are saved
#load("profileQC.rda")
#tail(profileQC, 3)