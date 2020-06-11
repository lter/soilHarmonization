#' @title Workflow to genrate units and conversions for location and profile
#'   data
#'
#' @description Table of units and conversions that the package uses to convert
#'   the unit of a user-supplied variable to a standardized unit if applicable.
#'   For example, the user can provide bulk layer CN data (lyr_soc) in units of
#'   %, g kg-1, mg kg-1, mg g-1, or ug g-1 but these must be standardized to, in
#'   this case, % for consistency across data sets. This table holds the
#'   conversion factor to convert lyr_soc from a unit other than % to %.
#'
#' @details Units conversion is applied as needed to both location and profile
#'   data. Initially, the conversion data for location and profile data were
#'   pulled from two seprate files (as they are in the source on Google Drive):
#'   unitsConversionLocation.rda and unitsConversionProfile.rda. We found,
#'   however, that we sometimes put location variables in the profile tab and
#'   vice versa, which resulted in those variables not being standardized as the
#'   package was looking for profile data in unitsConversionProfile.rda, for
#'   example. To address this, the two data tables (unitsConversionLocation.rda
#'   and unitsConversionProfile.rda) were merged into a single file
#'   (unitsConversions.rda) and the data_homogenization.R script updated
#'   accordingly. Ultimately, this approach too was lacking, discovered when we
#'   needed to update the agb and bgb units conversions. The update was
#'   addressed in the Google Sheet units_translation_table where the units
#'   conversion values where initially developed. To synchronize changes between
#'   that source, and the pacakge, I added the workflow to create the
#'   unitsConversions.rda file to this package as an unexported R file. See
#'   units_conversion_update.R in this package.
#'
#' @note The workflow is included and documeneted as part of the
#'   soilHarmonization package but is to be run sporadically and, at least at
#'   this time, not by other users thus is saved to the root directory of the
#'   package so that it is not loaded (per
#'   https://stackoverflow.com/questions/13182634/rbuildignore-and-excluding-directories)
#'
#' @note Data are read from LTER-SOM-keyfile/units_translation_table#'
#' @note New names are read from LTER-SOM-keyfile/KeyV2_newVarNames#'
#'
#' @import googledrive
#' @import tidyverse
#' @importFrom googlesheets4 read_sheet
#'
#' @return A tibble (then rda saved to the data directory) of variables and
#'   appropriate conversion values from provided to target values.

library(googlesheets4)
library(googledrive)
library(tidyverse)

options(scipen = 999)

newConversionLocation <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1NKCZu0wguoFg9ZTwKpWzYwWqKl_Ppal7k8zqGb8fk3Y/edit?usp=sharing",
  sheet = "Location_data")

newConversionProfile <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1NKCZu0wguoFg9ZTwKpWzYwWqKl_Ppal7k8zqGb8fk3Y/edit?usp=sharing",
  sheet = "Profile_data (Key-Key)")

newNames <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1HnM1wSguzFe7HEBMsOYrB2vyjs7RTRpfRaI4RxnLdsA/edit?usp=sharing")

unitsConversions <- bind_rows(
  newConversionLocation %>%
    left_join(newNames %>% select(-Var_long), by = c("var" = "var_old_name")) %>%
    mutate(var = case_when(
      !is.na(var_new_name) ~ var_new_name,
      TRUE ~ var
    )) %>%
    select(unit_levels = Unit, Var_long, var, givenUnit, unitConversionFactor) %>%
    filter(!is.na(unit_levels)),
  newConversionProfile %>%
    left_join(newNames %>% select(-Var_long), by = c("var" = "var_old_name")) %>%
    mutate(var = case_when(
      !is.na(var_new_name) ~ var_new_name,
      TRUE ~ var
    )) %>%
    filter(
      !grepl("fraction|profile", Level),
      !is.na(unitConversionFactor)
    ) %>%
    select(unit_levels, Var_long, var, givenUnit, unitConversionFactor)
)

save(unitsConversions, file = "data/unitsConversions.rda")
