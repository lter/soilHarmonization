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
row_to_add <- profileQC %>% filter(var == "bd_tot") %>%
                            mutate(var = "bd_tot_mdl") %>%
                            mutate(Var_long = "Bulk Density modeled, With Coarse Fragments") %>%
                            mutate(Level = "profile") %>%
                            mutate(minValue = -30) %>%
                            mutate(maxValue = 40) %>%
                            mutate(unit_levels = NA) %>%
                            mutate(givenUnit = "degC") %>%
                            mutate(hardUnit = "degC") 

profileQC <- profileQC %>% add_row(row_to_add)

#Save changes  
save(profileQC, file = "profileQC.rda")


# Check if changes are saved
#load("profileQC.rda")
#tail(profileQC, 3)

colnames(profileQC)
colnames(locationQC)
