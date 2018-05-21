#' @title Homogenize LTER Soil Organic Matter Working Group data and notes
#'
#' @description The function data_homogonization imports data and a key
#'   translation file from a Google Directory that the user is able to access.
#'   Data are homogenized to feature common header names and units, and
#'   dataset-level notes appended to each data file. Output includes
#'   standardized data and notes to a local directory on the user's computer
#'   that are also uploaded to the Google Directory identified by the user as
#'   containing the source data and key translation file. Modified data are
#'   appeneded with the string 'HMGZD'.
#'
#' @note data_homogonization relies on the helper functions detect_os and
#'   sheet_download.
#'
#' @param directoryName The quoted name of the Google Drive directory containing data and a key translation file
#' @param temporaryDirectory The quoted path and name of a temporary directory
#'   on the user's local computer for storing script output. The directory does
#'   not have to exist. The directory should end with a forward slash (Mac,
#'   Linux) or backward slash (Windows).
#'
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import magrittr
#' @import googledrive
#' @import googlesheets
#' @import tools
#' @importFrom stringr str_extract
#'
#' @return Homogenized data and notes in a local directory identified by the
#'   user, and uploaded to the Google Drive source directory.
#'
#' @examples
#' \dontrun{
#'
#'  data_homogonization(directoryName = 'CDR_E120',
#'                      temporaryDirectory = '~/Desktop/CRD_E120_output/')
#'
#' }
#'
#' @export

data_homogonization <- function(directoryName, temporaryDirectory) {

  # CHECK FOR REQUISITE PARAMETERS

  # directoryName
  if (missing(directoryName)) {
    stop("provide the name of a Google Drive directory")
  }

  # temporaryDirectory
  if (missing(temporaryDirectory)) {
    stop("provide the name of a local directory to house script output")
  }


  # LOCAL OUTPUT DIRECTORY

  # a user-identified temporaryDirectory is required

  # ensure the provided temporaryDirectory has a trailing slash
  os <- detect_os()

  if (os %in% c('lin', 'mac')) {

    if (stringr::str_extract(temporaryDirectory, ".$") != "/") {
      temporaryDirectory <- paste0(temporaryDirectory, "/")
    }

  } else if (os == 'win') {

    if (stringr::str_extract(temporaryDirectory, ".$") != "\\") {
      temporaryDirectory <- paste0(temporaryDirectory, "\\")
    }

  }

  # create the receiving directory if it does not exist; delete the contents if
  # it does exist
  if (!dir.exists(temporaryDirectory)) {

    dir.create(temporaryDirectory)

  } else {

    file.remove(file.path(temporaryDirectory, list.files(temporaryDirectory)))
  }


  # GOOGLE DRIVE DIRECTORY

  # access Google directory id for reference
  googleID <- googledrive::drive_get(directoryName) %>%
    pull(id)

  # list files in Google directory
  dirFileList <- googledrive::drive_ls(path = directoryName)

  # isolate names from Google directory
  dirFileNames <- dirFileList %>%
    select(name) %>%
    pull(name)


  # ACCESS KEY FILE

  # isolate key-key and extract details in location and profile tabs
  keyFileName <- grep("key", dirFileNames, ignore.case = T, value = T)
  keyFileToken <- googlesheets::gs_title(keyFileName)

  locationData <- googlesheets::gs_read(keyFileToken, ws = 1) %>%
    filter(!is.na(Value)) %>%
    add_row(Value = googleID, var = 'google_id', .before = 1)

  profileData <- googlesheets::gs_read(keyFileToken, ws = 2) %>%
    filter(!is.na(header_name))


  # GENERATE NOTE FILE (FROM THE KEY FILE)

  # create a note name with path to output directory, name of key file + _HMGZD_NOTES.csv
  notesFileName <- paste0(tools::file_path_sans_ext(keyFileName), "_HMGZD_NOTES.csv")

  # capture notes from location and profile key-file tabs
  notes <- bind_rows(
    locationData %>%
      filter(!is.na(var_notes)) %>%
      mutate(source = "location") %>%
      select(source, Var_long, var, var_notes),
    profileData %>%
      filter(!is.na(Notes) | !is.na(Comment)) %>%
      tidyr::unite(col = var_notes, Notes, Comment, sep = ";") %>%
      mutate(source = "profile") %>%
      select(source, Var_long, var, var_notes)
  )


  # +++++++++++++++++++++++++++++++++++++++
  # BEGIN STANDARDIZE UNITS::location data
  # source('~/Dropbox/development/standardize_units_location.R')

  # location tab DATA containing units only
  locationDataUnits <- locationData %>%
    filter(!is.na(Unit)) %>%
    select(Value, unit_levels = Unit, Var_long, var)

  # join location DATA with units and corresponding vars in conversion table
  LDU_UCL <- left_join(locationDataUnits, unitsConversionLocation,
                       by = c("var", "unit_levels"),
                       suffix = c(".PD", ".UT")) %>%
    filter(
      !is.na(unitConversionFactor),
      unitConversionFactor != 1
    )

  # standardize location data units
  for (varValue in c(LDU_UCL$var)) {

    # standardize values per the units_conversion_table
    locationData[locationData$var == varValue,]['Value'] <- as.numeric(locationData[locationData$var == varValue,]['Value']) * LDU_UCL[LDU_UCL$var == varValue,]['unitConversionFactor']

    # add mention of conversions to notes
    if (nrow(notes[notes$var == varValue,]) >= 1) {
      notes <- notes %>%
        mutate(var_notes = replace(var_notes,
                                   var == varValue,
                                   paste0(var_notes, "; UNITS CONVERSION APPLIED: ", LDU_UCL[LDU_UCL$var == varValue,]['unitConversionFactor'])))
    } else {
      notes <- notes %>%
        add_row(source = 'location',
                var = varValue,
                var_notes = paste0("UNITS CONVERSION APPLIED: ", LDU_UCL[LDU_UCL$var == varValue,]['unitConversionFactor']))
    }
  }

  # END STANDARDIZE UNITS::location data
  # +++++++++++++++++++++++++++++++++++++++


  # IMPORT DATA

  # set import parameters

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
  if (exists('mvc1') && exists('mvc2')) { missingValueCode = c(paste(mvc1, mvc2))}


  # DATA FILE(S)

  # import all (data + key) files from google dir
  googleDirData <- lapply(dirFileNames,
                          sheet_download,
                          missingValueCode = missingValueCode,
                          skipRows = skipRows)

  # add filenames
  names(googleDirData) <- dirFileNames

  # as key file is already loaded, remove it from the list of data frames
  googleDirData <- googleDirData[-grepl("key", names(googleDirData), ignore.case = T)]

  # generate a vector of dataframe columns to keep from key file input to
  # header_name
  varsToKeep <- profileData %>%
    select(header_name) %>%
    pull()

  # pull targeted vars from each data frame based on header_names in key file
  googleDirData <- purrr::map(googleDirData, select, one_of(varsToKeep))

  # rename investigator names to key file names
  googleDirData <- lapply(googleDirData, function(frame) {
    setNames(frame, profileData$var[match(names(frame), profileData$header_name)]) })


  # +++++++++++++++++++++++++++++++++++++++
  # BEGIN STANDARDIZE UNITS::profile data
  # source('~/Dropbox/development/standardize_units_profile.R')

  # profile tab DATA containing units only
  profileDataUnits <- profileData %>%
    filter(!is.na(unit_levels)) %>%
    select(header_name, unit_levels, Var_long, var)

  # join profile DATA with units and corresponding vars in conversion table
  PDU_UCP <- left_join(profileDataUnits, unitsConversionProfile,
                       by = c("var", "unit_levels"),
                       suffix = c(".PD", ".UT")) %>%
    filter(
      !is.na(unitConversionFactor),
      unitConversionFactor != 1
    )

  # loop through all data frames in google dir
  for (i in 1:length(googleDirData)) {

    for (dataCol in c(PDU_UCP$var)) {

      if (!is.null(googleDirData[[i]][[dataCol]])) {

        googleDirData[[i]][[dataCol]] <- googleDirData[[i]][[dataCol]] * PDU_UCP[PDU_UCP$var == dataCol,][['unitConversionFactor']]

        # add mention of conversions to notes
        if (nrow(notes[notes$var == dataCol,]) >= 1) {
          print(paste("mutate: ", dataCol))

          baseNote <- notes %>%
            filter(var == dataCol) %>%
            select(var_notes)

          notes <- notes %>%
            mutate(var_notes = replace(var_notes,
                                       var == dataCol,
                                       paste0(baseNote, "; UNITS CONVERSION APPLIED: ", PDU_UCP[PDU_UCP$var == dataCol,]['unitConversionFactor'])))
        } else {
          print(paste("add_row: ", dataCol))
          notes <- notes %>%
            add_row(source = 'profile',
                    var = dataCol,
                    var_notes = paste0("UNITS CONVERSION APPLIED: ", PDU_UCP[PDU_UCP$var == dataCol,]['unitConversionFactor']))
        }
      }
    }
  }

  # END STANDARDIZE UNITS::profile data
  # +++++++++++++++++++++++++++++++++++++++

  # generate wide data frame of location data
  locationDataWide <- locationData %>%
    select(var, Value) %>%
    tidyr::spread(key = var, value = Value)

  # merge location data with each data frame
  googleDirData <- lapply(googleDirData, function(frame) {
    merge(locationDataWide, frame, all = T) })

  # rename files to include base name + indication of homogenization
  names(googleDirData) <- paste0(stringr::str_extract(names(googleDirData), "^[^\\.]*"), "_HMGZD")


  # FILE OUTPUT

  # notes to temporary location
  readr::write_csv(notes, paste0(temporaryDirectory, notesFileName))

  # data to temporary location
  googleDirData %>%
    names(.) %>%
    purrr::map(~ readr::write_csv(googleDirData[[.]], paste0(temporaryDirectory, ., ".csv")))


  # UPLOAD OUTPUT TO GOOGLE DRIVE

  # identify directory with files (not full.names=T)
  filesToUpload <- list.files(path = temporaryDirectory,
                              full.names = FALSE,
                              recursive = FALSE)

  # upload these files to the target Google directory
  lapply(filesToUpload, function(frame) {
    googledrive::drive_upload(paste0(temporaryDirectory, frame),
                              path = googledrive::drive_get(googledrive::as_id(googleID)),
                              type = "spreadsheet")
  })

}
