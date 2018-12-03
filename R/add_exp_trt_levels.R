#' @title Add experiment and treatment unit levels to homogenized output
#'
#' @description The add_exp_trt_levels function adds the unit levels (e.g.,
#'   Plot, Watershed, warming, precip) as new columns in the homogenized output.
#'   New columns employ the naming convention of experiment or treatment level
#'   '_level' (e.g., L2_level).
#'
#' @note add_exp_trt_levels is intended as a helper function for the main
#'   data_homogenization.R of the soilHarmonization package. As such, this
#'   function requires the profileData object that is part of the
#'   data_harmonization.R workflow and will not work outside of that workflow.
#'
#' @param frame a data frame or, more likely, a date frame in a list
#'
#' @import dplyr
#'
#' @return A modified object of type Tibble or data frame
#'
#' @export
#

add_exp_trt_levels <- function(frame, profileData, experimentTreatmentVarSet) {

  # pare profileData to experiment and treatment variables, AND only those in
  # the target data frame; add a new column ('var_level'), which will be the
  # name of the new column containing the level type in the data
  profileDataExpTrt <- profileData %>%
    filter(
      var %in% experimentTreatmentVarSet,
      var %in% colnames(frame)
    ) %>%
    mutate(var_level = paste0(var, "_level")) %>%
    select(unit_levels, var, var_level)

  # loop through a given frame to add the corresponding levels for each
  # experiment or treatment variable

  # developer note, dplyr dynamic name typing:
  # https://stackoverflow.com/questions/26003574/dplyr-mutate-use-dynamic-variable-names

  for (i in 1:nrow(profileDataExpTrt)) {

    newColName <- profileDataExpTrt[i,][['var_level']]
    newColValue <- profileDataExpTrt[i,][['unit_levels']]
    newPosition <- profileDataExpTrt[i,][['var']]

    frame <- add_column(
      .data = frame,
      !!newColName := newColValue,
      .after = newPosition
    )

  } # close loop

  return(frame)

} # close add_exp_trt_levels
