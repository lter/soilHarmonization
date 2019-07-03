#' @title Catalog location- and profile-data variables that are candidates for
#'   units conversion but for which a units conversion was not applied.
#'
#' @description The function vars_not_converted catalogs location- and
#'   profile-data variables that are candidates for units conversion but for
#'   which a units conversion was not applied. vars_not_converted is a helper
#'   function that is called within data_homogenization(). Output is bound to
#'   the convesionNotes objet, generated in data_homogenization() and documented
#'   in the homogenization notes file (PDF).
#'
#' @param varType The type of variable ("location", "profile") passed as a
#'   string to be documented.
#'
#' @import dplyr
#'
#' @return A tibble or data frame object that is merged with other conversion
#'   notes, and included in the homogenization report file (PDf).
#'
#' @examples
#' \dontrun{
#'
#'  vars_not_converted("location"),
#'  vars_not_converted("profile")
#'
#'  bind_rows(
#'    conversionNotes,
#'    vars_not_converted("location"),
#'    vars_not_converted("profile")
#'   )
#'
#' }
#'
#' @export

vars_not_converted <- function(varType,
                               locationDataUnits,
                               unitsConversionLocation,
                               LDU_UCL,
                               profileDataUnits,
                               unitsConversionProfile,
                               PDU_UCP) {

  if (varType == "location") {

    unitsType <- locationDataUnits
    conversionType <- unitsConversionLocation
    joinedUnits <- LDU_UCL
    sourceType <- "location"

  } else {

    unitsType <- profileDataUnits
    conversionType <- unitsConversionProfile
    joinedUnits <- PDU_UCP
    sourceType <- "profile"

  }

  varsNotConverted <- left_join(unitsType, conversionType,
                                by = c("var"),
                                suffix = c(".PD", ".UT")) %>%
    dplyr::filter(
      !is.na(unitConversionFactor),
      unitConversionFactor != 1
    ) %>%
    filter(!var %in% joinedUnits$var) %>%
    group_by(unit_levels.PD, var) %>%
    summarise(
      Var_long = max(Var_long.PD),
      target_unit = max(givenUnit)
    ) %>%
    mutate(
      source = sourceType,
      varNotes = "NOT converted"
    ) %>%
    select(
      source,
      var,
      Var_long,
      given_unit = unit_levels.PD,
      target_unit,
      varNotes
    )

  return(varsNotConverted)

}
