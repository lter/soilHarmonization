#' @title Table of units and conversions for location and profile data
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
#'   accordingly. unitsConversionLocation.rda and unitsConversionProfile.rda
#'   were left in the package although not used in case unitsConversions.rda
#'   needs to be edited or rebuilt and in case the individual files for that are
#'   necessary. The two tables were combined into one following:
#'
#' unitsConversions <- bind_rows(unitsConversionLocation,
#' unitsConversionProfile)
#' save(unitsConversions, file="data/unitsConversions.rda")
#'
#' @format A data frame with 371 rows and 5 variables:
#' \describe{
#'   \item{unit_levels}{units available to user from key file input}
#'   \item{Var_long}{long-name or description of variable}
#'   \item{var}{short-name of variable}
#'   \item{givenUnit}{desired unit for variable}
#'   \item{unitConversionFactor}{multiplier to convert unit from provided unit (unit_levels) to target unit (givenUnit)}
#' }
#'
#' @source
#'   \href{https://drive.google.com/open?id=1NKCZu0wguoFg9ZTwKpWzYwWqKl_Ppal7k8zqGb8fk3Y}{units_translation_table}
#'
"unitsConversions"
