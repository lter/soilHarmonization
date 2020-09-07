library(tidyverse)

### unitsConversion ###
#########################
setwd('C:/github/RCsoilHarmonization/data')

load("unitsCOnversions.rda")

new_units <- unitsConversions %>% filter(var == "n_min") %>% 
                                  mutate(var = "n_min_pot") %>%
                                  mutate(Var_long = "Net N mineralization at water holding capacity")

unitsConversions_updated <- unitsConversions %>% add_row(new_units)

save(unitsConversions_updated, file = "unitsCOnversions.rda")


# Check if changes are saved
#load("profileQC.rda")
#tail(profileQC, 3)