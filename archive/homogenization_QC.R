#' @title Processing summary and quality control checks for homogenized LTER
#'   Soil Organic Matter Working Group data
#'
#' @description The function homogonization_QC imports provided and homogenized
#'   data, and the associated key translation file from a Google Directory that
#'   the user is able to access. The script compares the provided data to the
#'   homogenized data, and reports on processing output, select quality control
#'   checks, and provides generalized plots of independent versus dependent
#'   variables. Output is a PDF report.
#'
#' @note data_homogonization relies on the helper functions detect_os and
#'   sheet_download.
#'
#' @param directoryName The quoted name of the Google Drive directory containing
#'   data and a key translation file
#' @param temporaryDirectory The quoted path and name of a temporary directory
#'   on the user's local computer for storing script output. The directory does
#'   not have to exist. The directory should end with a forward slash (Mac,
#'   Linux) or backward slash (Windows).
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import googledrive
#' @import googlesheets
#' @import tools
#' @importFrom stringr str_extract
#' @import pander
#' @importFrom fuzzyjoin stringdist_inner_join
#' @import purrr
#' @import ggplot2
#' @import rmarkdown
#'
#' @return A report in PDF format of processing summary, quality control checks,
#'   and generalized plots of homogenized data in a local directory identified
#'   by the user.
#'
#' @examples
#' \dontrun{
#'
#'  homogenization_QC(directoryName = 'CDR_E120',
#'                    temporaryDirectory = '~/Desktop/CRD_E120_output/')
#'
#' }
#'
#' @export

homogenization_QC <- function(directoryName, temporaryDirectory) {

  # requisite parameters ----------------------------------------------------

  # directoryName
  if (missing(directoryName)) {
    stop("provide the name of a Google Drive directory")
  }

  # temporaryDirectory
  if (missing(temporaryDirectory)) {
    stop("provide the name of a local directory to house script output")
  }


  # local directory ---------------------------------------------------------

  # create the receiving directory if it does not exist; delete the contents if
  # it does exist
  if (!dir.exists(temporaryDirectory)) {
    dir.create(temporaryDirectory)
  } else {
    file.remove(file.path(temporaryDirectory, list.files(temporaryDirectory)))
  }

  # ensure the provided temporaryDirectory has a trailing slash
  if (stringr::str_extract(temporaryDirectory, ".$") != "/") {
    temporaryDirectory <- paste0(temporaryDirectory, "/")
  }


  # google drive directory --------------------------------------------------

  # access Google directory id for reference
  googleID <- googledrive::drive_get(directoryName) %>%
    dplyr::pull(id)

  # list files in Google directory
  # suppressWarnings to ignore googleDrive ref. to collapse, which is deprecated
  dirFileList <- suppressWarnings(googledrive::drive_ls(path = directoryName))

  # isolate names of files in Google directory
  dirFileNames <- dirFileList %>%
    filter(
      !grepl("duplicates", name), # remove duplicates file/folder
      file_ext(name) != "zip", # remove zipped files
      file_ext(name) != "pdf", # remove PDF files
      file_ext(name) != "html", # remove html files
      file_ext(name) != "txt" # remove txt files
    ) %>%
    select(name) %>%
    pull(name)

  # confirm homogenized files exist
  if (length(dirFileNames[grep("HMGZD", dirFileNames)]) == 0) {
    stop("homogenized files not found - check that this directory was processed")
  }

  # isolate homogenized and non-homogenized files
  hmgzdFiles <- dirFileNames[grep("HMGZD", dirFileNames)]
  oeFiles <- dirFileNames[-grep("HMGZD", dirFileNames)]


  # access key file ---------------------------------------------------------

  # isolate key-key and extract details in location and profile tabs
  keyFileName <- grep("key", oeFiles, ignore.case = T, value = T)
  keyFileToken <- googlesheets::gs_title(keyFileName)

  locationData <- googlesheets::gs_read(keyFileToken, ws = 1) %>%
    dplyr::filter(!is.na(Value)) %>%
    tibble::add_row(Value = googleID, var = 'google_id', .before = 1)

  profileData <- googlesheets::gs_read(keyFileToken, ws = 2) %>%
    dplyr::filter(!is.na(header_name))


  # import data -------------------------------------------------------------

  # SET IMPORT PARAMETERS

  # Isolate rows to skip from locationData for data import. This was originally
  # intended to be an input as to the number of rows to skip but it seems to
  # have been interpreted by users as the row number of the header.
  if(length(locationData[locationData$var == 'header_row',]$var) == 1) {
    skipRows = as.numeric(locationData[locationData$var == 'header_row',]$Value) - 1
  } else {
    skipRows = 0
  }

  # isolate missing value codes from locationData for data import
  if (length(locationData[locationData$var == 'NA_1',]$var) == 1) {
    mvc1 = locationData[locationData$var == 'NA_1',]$Value }
  if (length(locationData[locationData$var == 'NA_2',]$var) == 1) {
    mvc2 = locationData[locationData$var == 'NA_2',]$Value }

  missingValueCode = "NA"
  if (exists('mvc1')) { missingValueCode = mvc1}
  if (exists('mvc2')) { missingValueCode = mvc2}
  if (exists('mvc1') && exists('mvc2')) { missingValueCode = c(mvc1, mvc2)}


  # HOMOGENIZED DATA FILE(S)

  # import all homogenized (data + key) files from google dir
  hmgzdData <- lapply(hmgzdFiles,
                      sheet_download)

  # add filenames
  names(hmgzdData) <- hmgzdFiles

  # remove homogenized notes
  hmgzdData <- hmgzdData[!grepl("key", names(hmgzdData), ignore.case = T)]


  # OE DATA FILE(S)

  # import all non-homogenized (data + key) files from google dir
  oeData <- lapply(oeFiles,
                   sheet_download,
                   missingValueCode = missingValueCode,
                   skipRows = skipRows)

  # add filenames
  names(oeData) <- oeFiles

  # key file was loaded in a previous step so remove from oe data
  oeData <- oeData[!grepl("key", names(oeData), ignore.case = T)]


  # OE and HMGZD data will not necessarily be loaded in step; use fuzzy matching
  # to build a tibble of OE data names that correspond to the HMGZD data names

  # Update 2018-10-29: because the number of string substitutions required for
  # matching will vary, the max_dist parameter in the fuzzy join will have to be
  # dynamic. New code sets the max_dist parameter to a low, default value of 5
  # and establishes an empty tibble of the matchedNames object. The
  # match_oe_hmgzd function is called with an incremenetally increasing max_dist
  # until the number of matches (rows in the matchedNames tibble) is equal to
  # the length of oeData objects (oeData chosen arbritatrily over hmgzdData -
  # and perhaps there should be some check that these are equal from the
  # outset).

  stringMaxDist <- 5 # arbitrary max_dist parameter
  matchedNames <- tibble() # empty tibble (nrow = 0)

  match_oe_hmgzd <- function(stringMaxDist) {

    matchedNamesInner <- tibble(oeNames = names(oeData)) %>%
      fuzzyjoin::stringdist_inner_join(tibble(hmgzdNames = names(hmgzdData)), by = c("oeNames" = "hmgzdNames"), max_dist = stringMaxDist)

    return(matchedNamesInner)

  }

  while(nrow(matchedNames) < length(oeData)) {

    stringMaxDist <- stringMaxDist + 1

    matchedNames <- match_oe_hmgzd(stringMaxDist)

  }


  # check I: number rows ----------------------------------------------------

  rowCount <- data.frame(data_entity = NA,
                         num_rows_data = NA,
                         num_rows_homogenized = NA)

  for (i in 1:length(oeData)) {

    rowCount[i,]$data_entity <- names(oeData)[[i]]
    rowCount[i,]$num_rows_data <- nrow(oeData[[i]])
    rowCount[i,]$num_rows_homogenized <- nrow(hmgzdData[[matchedNames[matchedNames$oeNames == names(oeData)[[i]],]$hmgzdNames]])

  }

  row.names(rowCount) <- NULL


  # check II: all location vars in hmgzd data -------------------------------

  locationDataCheck <- function(homogenizedDataEntity) {

    numLocationHmgzdJoin <- locationData %>%
      select(nameHMGZD = var, providedValue = Value) %>%
      left_join(
        homogenizedDataEntity %>%
          slice(1:1) %>%
          gather(key = varHMGZD, value = valueHMGZD), by = c("nameHMGZD" = "varHMGZD")
      ) %>%
      filter(!is.na(valueHMGZD)) %>%
      tally()

    if (nrow(locationData) == numLocationHmgzdJoin) {

      locationDataCheckResult <- data.frame(data_check = "all provided location variables included")

    } else {

      #
      # need to add a column indicating what the check is and can we add the data entity name?
      #
      locationDataCheckResult <- locationData %>%
        select(nameHMGZD = var, providedValue = Value) %>%
        left_join(
          homogenizedDataEntity %>%
            slice(1:1) %>%
            gather(key = varHMGZD, value = valueHMGZD), by = c("nameHMGZD" = "varHMGZD")
        ) %>%
        filter(is.na(valueHMGZD))

    }

    return(locationDataCheckResult)

  }

  locationDataCheckResult <- purrr::map_df(hmgzdData, locationDataCheck, .id = "data_source")


  # check III: all profile vars in hmgzd data -------------------------------

  profileDataSummary <- function(homogenizedDataEntity) {

    profileDataSummary <- profileData %>%
      select(nameHMGZD = var, providedValue = header_name) %>%
      left_join(
        homogenizedDataEntity %>%
          summarise_all(funs(max), na.rm = TRUE) %>%
          gather(key = varHMGZD, value = valueHMGZD), by = c("nameHMGZD" = "varHMGZD")
      ) %>%
      filter(!is.na(valueHMGZD)) %>%
      select(
        providedName = providedValue,
        homogenizedName = nameHMGZD,
        exampleValue = valueHMGZD
      )

    return(profileDataSummary)

  }

  profileDataSummaryResult <- purrr::map_df(hmgzdData, profileDataSummary, .id = "data_source")


  # QA plots ----------------------------------------------------------------

  # resources used in plotting calls:

  # vector of all possible treatment and experiment vars from key file
  allExTrtLevels <- c("L1", "L2", "L3", "L4", "L5", "tx_L1", "tx_L2", "tx_L3", "tx_L4", "tx_L5", "observation_date")

  # vector of all location variables
  locationVars <- locationData %>%
    filter(!is.na(Value)) %>%
    select(var) %>%
    pull()

  # vector of experiment and treatment variables in data set
  exTrtVars <- profileData %>%
    filter(
      !is.na(header_name),
      var %in% allExTrtLevels
    ) %>%
    select(var) %>%
    pull()

  # vector of dependent variables in data set
  dependentVars <- profileData %>%
    filter(
      !is.na(header_name),
      !var %in% allExTrtLevels
    ) %>%
    select(var) %>%
    pull()

  # HELPER FUNCTION not_all_na: for removing empty columns for plotting
  not_all_na <- function(x) {!all(is.na(x))}

  # HELPER FUNCTION ggname: to surround dependent variable passed to plotting
  # function in backticks. This problem arose when attempting to plot data sets
  # that had column nanes starting with a number (e.g., 13c) because we need to
  # call aes_string (as opposed to aes).

  # https://stackoverflow.com/questions/13445435/ggplot2-aes-string-fails-to-handle-names-starting-with-numbers-or-containing-s

  ggname <- function(x) {
    if (class(x) != "character") {
      return(x)
    }
    y <- sapply(x, function(s) {
      if (!grepl("^`", s)) {
        s <- paste("`", s, sep="", collapse="")
      }
      if (!grepl("`$", s)) {
        s <- paste(s, "`", sep="", collapse="")
      }
    }
    )
    y
  }

  # faceted plot of differing types from:

  # complicated approaches (worked but ugly)
  # https://statbandit.wordpress.com/2011/07/29/a-ggplot-trick-to-plot-different-plot-types-in-facets/
  # https://stackoverflow.com/questions/7903972/can-you-specify-different-geoms-for-different-facets-in-a-ggplot

  # simple approach:
  # https://stackoverflow.com/questions/6750664/how-to-change-the-format-of-an-individual-facet-wrap-panel

  # qcPlotsInner

  # NOTE: function uses a different approach depending on whether any
  # independent variables are numeric, character, or a mix of numeric and
  # character.

  # NOTE: as constructed, function will fail if there are not any independent
  # variables in the data file

  qcPlotsInner <- function(dataEntity, depVar) {

    # VECTOR of indendent variables in data entity (with at least one value)
    hmgzdIndVars <- dataEntity %>%
      select(one_of(exTrtVars)) %>%
      select_if(not_all_na) %>%
      colnames()

    if (all(sapply(dataEntity[,names(dataEntity) %in% c(exTrtVars)], is.numeric))) {

      # panels of box plots for passed dependent variable if all independent
      # variables are numeric

      # first call ggname helper function to surround depVar in backticks to
      # accommodate vars that start with numbers; here we need a unticked (for
      # dplyr) and ticked (for ggplot::aes_string) version of dep var so create
      # a unique parameter (depVarTicked) to pass only to ggplot
      depVarTicked <- ggname(depVar)

      dataEntity %>%
        select(one_of(hmgzdIndVars), depVar) %>%
        gather(key = varHMGZD, value = valueHMGZD, -depVar) %>%
        ggplot(aes_string(x = "valueHMGZD", y = depVarTicked)) +
        geom_point() +
        facet_wrap(~ varHMGZD, scales = "free")

    } else if (all(sapply(dataEntity[,names(dataEntity) %in% c(exTrtVars)], is.character))) {

      # panels of box plots for passed dependent variable if all independent
      # variables are non-numeric

      # first call ggname helper function to surround depVar in backticks to
      # accommodate vars that start with numbers; here we need a unticked (for
      # dplyr) and ticked (for ggplot::aes_string) version of dep var so create
      # a unique parameter (depVarTicked) to pass only to ggplot
      depVarTicked <- ggname(depVar)

      dataEntity %>%
        select(one_of(hmgzdIndVars), depVar) %>%
        gather(key = varHMGZD, value = valueHMGZD, -depVar) %>%
        ggplot(aes_string(x = "valueHMGZD", y = depVarTicked)) +
        geom_boxplot() +
        facet_wrap(~ varHMGZD, scales = "free")

    } else {

      # panels of mixed box and point plots for passed dependent variable when
      # there is a mix of numeric and non-numeric indepdnent variables

      # isolate non-numeric independent variables
      characterVars <- dataEntity %>%
        select(one_of(exTrtVars)) %>%
        select_if(not_all_na) %>%
        select_if(is_character) %>%
        colnames()

      # isolate numeric independent variables
      numericVars <- dataEntity %>%
        select(one_of(exTrtVars)) %>%
        select_if(not_all_na) %>%
        select_if(is_numeric) %>%
        colnames()

      # generate data set for passed dependent variable (depVar), non-numeric
      characterData <- dataEntity %>%
        select(one_of(hmgzdIndVars), depVar) %>%
        gather(key = varHMGZD, value = valueHMGZD, -depVar) %>%
        filter(
          varHMGZD %in% characterVars,
          !is.na(valueHMGZD),
          !is.na(depVar))

      # generate data set for passed dependent variable (depVar), numeric
      numericData <- dataEntity %>%
        select(one_of(hmgzdIndVars), depVar) %>%
        gather(key = varHMGZD, value = valueHMGZD, -depVar) %>%
        filter(
          varHMGZD %in% numericVars,
          !is.na(valueHMGZD),
          !is.na(depVar)) %>%
        mutate(valueHMGZD = as.numeric(valueHMGZD))

      # generate plot

      # first call ggname helper function to surround depVar in backticks to
      # accommodate vars that start with numbers
      depVarTicked <- ggname(depVar)

      characterData %>%
        ggplot(aes_string(y = depVarTicked)) +
        geom_boxplot(aes_string(x = "valueHMGZD")) +
        geom_point(data = numericData, aes_string(x = "valueHMGZD")) +
        facet_wrap(~ varHMGZD, scales = "free")

    }

  } # close qcPlotsInner

  # qcPlotsOuter generates plots of all independent vars v. each dependent var
  # for each data entity in the data set
  qcPlotsOuter <- function(dataEntity) {

    # LIST of indendent variables in data entity (with at least one value)
    hmgzdDepVars <- dataEntity %>%
      select(one_of(dependentVars)) %>%
      select_if(not_all_na) %>%
      colnames() %>%
      as.list()

    lapply(hmgzdDepVars, qcPlotsInner, dataEntity = dataEntity)

  }

  # generate plot output
  plotOutput <- lapply(hmgzdData, qcPlotsOuter)


  # call render -------------------------------------------------------------

  rmarkdown::render(input = system.file("homogenizationQCR.Rmd", package = "soilHarmonization"),
                    params = list(
                      param_namesOE = sort(names(oeData)),
                      param_namesHmgzd = sort(names(hmgzdData)),
                      param_rowCount = rowCount,
                      param_locationVars = locationDataCheckResult,
                      param_numProfileOE = nrow(profileData),
                      param_numProfileHZ = nrow(profileDataSummaryResult),
                      param_profileVars = profileDataSummaryResult,
                      param_summaryPlots = plotOutput
                    ),
                    output_file = paste0(temporaryDirectory, directoryName, "_HMGZD_QC.html"))



  # upload html output to Google Drive --------------------------------------

  googledrive::drive_upload(paste0(temporaryDirectory, directoryName, "_HMGZD_QC.html"),
                            path = googledrive::drive_get(googledrive::as_id(googleID)))


} # end of homogenization_QC function
