library(tidyverse)

### profileQC ###
#########################
setwd('C:/github/RCsoilHarmonization/data')

load("profileQC.rda")

#Option 1: Full info entry
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


#Option 2: Copy and modify
row_to_add <- profileQC %>% filter(var == "n_min") %>%
                            mutate(var = "n_min_pot") %>%
                            mutate(Var_long = "Net N mineralization at water holding capacity")

profileQC <- profileQC %>% add_row(row_to_add)


#Save changes  
save(profileQC, file = "profileQC.rda")


# Check if changes are saved
#load("profileQC.rda")
#tail(profileQC, 3)

