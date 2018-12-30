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
#' @import tibble
#' @import googledrive
#' @import googlesheets
#' @import tools
#' @importFrom stringr str_extract
#' @importFrom purrr map_df
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

data_homogenization <- function(directoryName, temporaryDirectory) {

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
  # os <- detect_os()

  # if (os %in% c('lin', 'mac')) {

  if (stringr::str_extract(temporaryDirectory, ".$") != "/") {
    temporaryDirectory <- paste0(temporaryDirectory, "/")
  }

  # } else if (os == 'win') {
  #
  #   if (stringr::str_extract(temporaryDirectory, ".$") != "\\") {
  #     temporaryDirectory <- paste0(temporaryDirectory, "\\")
  #   }
  #
  # }

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
    dplyr::pull(id)

  # list files in Google directory
  dirFileList <- googledrive::drive_ls(path = directoryName)

  # isolate names from Google directory
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


  # KEY FILE

  # Update 2018-12-28: data_harmonization requires a key file version 2
  if (!any(grepl("KEY_V2", dirFileNames, ignore.case = F))) {

    stop("data_harmonization requires a key file version 2")

  }

  # isolate key file, and extract details in location and profile tabs
  keyFileName <- grep("KEY_V2", dirFileNames, ignore.case = F, value = T)
  keyFileToken <- googlesheets::gs_title(keyFileName)

  # extract location and profile tabs of key file
  locationData <- googlesheets::gs_read(keyFileToken, ws = 1)
  profileData <- googlesheets::gs_read(keyFileToken, ws = 2)

  # LOCATION TAB QC

  # (1) confirm requisite input to location tab
  locationRequiredFields <- c(
    'curator_PersonName',
    'curator_organization',
    'curator_email',
    'time_series',
    'gradient',
    'experiments',
    'merge_align'
  )

  if(any(is.na(locationData[locationData[['var']] %in% locationRequiredFields,]['Value']))) {

    print(locationData[locationData[['var']] %in% locationRequiredFields,][c('var', 'Value')])
    stop("at least one required field in location tab is missing (see output for missing (NA) value)")

  }


  # lat long coordinates
  # wait for Derek's code to address lat long


  # remove missing fields and add Google reference details to location
  locationData <- locationData %>%
    dplyr::filter(!is.na(Value)) %>%
    tibble::add_row(Value = googleID, var = 'google_id') %>%
    tibble::add_row(Value = directoryName, var = 'google_dir')

  # remove missing fields from profile
  profileData <- profileData %>%
    dplyr::filter(!is.na(header_name))


  # GENERATE NOTE FILE (FROM THE KEY FILE)

  # create a note name with path to output directory, name of key file + _HMGZD_NOTES.csv
  notesFileName <- paste0(tools::file_path_sans_ext(keyFileName), "_HMGZD_NOTES.csv")

  # capture notes from key file location and profile tabs
  notes <- dplyr::bind_rows(
    tibble(
      source = "location",
      Var_long = "Google Directory",
      var = NA,
      var_notes = directoryName
    ),
    locationData %>%
      filter(var %in% c('network', 'site_code', 'location_name')) %>%
      dplyr::mutate(source = "location") %>%
      dplyr::select(source, Var_long, var, var_notes = Value),
    locationData %>%
      dplyr::filter(!is.na(var_notes)) %>%
      dplyr::mutate(source = "location") %>%
      dplyr::select(source, Var_long, var, var_notes),
    profileData %>%
      dplyr::filter(!is.na(Notes) | !is.na(Comment)) %>%
      tidyr::unite(col = var_notes, Notes, Comment, sep = ";") %>%
      dplyr::mutate(source = "profile") %>%
      dplyr::select(source, Var_long, var, var_notes)
  )

  # +++++++++++++++++++++++++++++++++++++++
  # BEGIN STANDARDIZE UNITS::location data
  # source('~/Dropbox/development/standardize_units_location.R')

  # location tab DATA containing units only
  locationDataUnits <- locationData %>%
    dplyr::filter(!is.na(Unit)) %>%
    dplyr::select(Value, unit_levels = Unit, Var_long, var)

  # join location DATA with units and corresponding vars in conversion table
  LDU_UCL <- dplyr::left_join(locationDataUnits, unitsConversionLocation,
                              by = c("var", "unit_levels"),
                              suffix = c(".PD", ".UT")) %>%
    dplyr::filter(
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
        dplyr::mutate(var_notes = replace(var_notes,
                                          var == varValue,
                                          paste0(var_notes, "; UNITS CONVERSION APPLIED: ", LDU_UCL[LDU_UCL$var == varValue,]['unitConversionFactor'])))
    } else {
      notes <- notes %>%
        tibble::add_row(source = 'location',
                        var = varValue,
                        var_notes = paste0("UNITS CONVERSION APPLIED: ", LDU_UCL[LDU_UCL$var == varValue,]['unitConversionFactor']))
    }
  }

  # END STANDARDIZE UNITS::location data
  # +++++++++++++++++++++++++++++++++++++++


  # +++++++++++++++++++++++++++++++++++++++
  # BEGIN QC CHECK::location data

  # establish empty tibble to log location QC errors
  location_QC_report <- tibble(
    var = as.character(NULL),
    error = as.character(NULL)
  )

  # function to check for location vars in prescribed range
  location_range_check <- function(locationVar) {

    tryCatch({

      targetValue <- locationQC %>%
        filter(!is.na(minValue)) %>%
        inner_join(locationData, by = c("var")) %>%
        filter(var == locationVar) %>%
        mutate(Value = as.numeric(Value))

      if (targetValue$Value < targetValue$minValue | targetValue$Value > targetValue$maxValue) {

        location_QC_report %>%
          add_row(
            var = locationVar,
            error = "out of range"
          )

      }

    },
    warning = function(cond) {

      return(
        location_QC_report %>%
          add_row(
            var = locationVar,
            error = "expected numeric"
          )
      )

    })

  } # close location_range_check

  # function to check provided data are appropriate type (numeric, character)
  location_type_check <- function(locationVar) {

    targetValue <- locationQC %>%
      filter(!is.na(class)) %>%
      inner_join(locationData, by = c("var")) %>%
      filter(var == locationVar)

    if (targetValue[['class']] == 'numeric') {

      tryCatch({

        locationData %>%
          filter(var == locationVar) %>%
          mutate(Value = as.numeric(Value))

        NULL

      },
      warning = function(cond) {

        return(
          location_QC_report %>%
            add_row(
              var = locationVar,
              error = "expected numeric"
            )
        )

      })

    } else if (targetValue[['class']] == 'character') {

      tryCatch({

        locationData %>%
          filter(var == locationVar) %>%
          mutate(Value = as.character(Value))

        NULL

      },
      warning = function(cond) {

        return(
          location_QC_report %>%
            add_row(
              var = locationVar,
              error = "expected character"
            )
        )

      })

    } else {

      NULL

    } # close if character

  } # close location_type_check

  # map through range and type checks for location data
  location_QC_report <- map_df(.x = locationQC %>% filter(!is.na(minValue)) %>% inner_join(locationData, by = c("var")) %>% filter(!is.na(Value)) %>% pull(var),
                               .f = location_range_check)
  location_QC_report <- map_df(.x = locationQC %>% filter(!is.na(class)) %>% inner_join(locationData, by = c("var")) %>% filter(!is.na(Value)) %>% pull(var),
                               .f = location_type_check)

  # report and exit if location QC errors detected
  if (nrow(location_QC_report) > 0) {

    location_QC_report <- location_QC_report %>%
      group_by(var, error) %>%
      distinct() %>%
      mutate(
        dataset = directoryName,
        source = 'location'
      ) %>%
      select(dataset, source, var, error)

    print(location_QC_report)
    stop("location errors detected")

  }

  # END QC CHECK::location data
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
  if (exists('mvc1') && exists('mvc2')) { missingValueCode = c(mvc1, mvc2)}


  # DATA FILE(S)

  # import all (data + key) files from google dir
  googleDirData <- lapply(dirFileNames,
                          sheet_download,
                          missingValueCode = missingValueCode,
                          skipRows = skipRows)

  # add filenames
  names(googleDirData) <- dirFileNames

  # as key file is already loaded, remove it from the list of data frames
  # googleDirData <- googleDirData[-grepl("key", names(googleDirData), ignore.case = T)]
  googleDirData <- googleDirData[!grepl("key", names(googleDirData), ignore.case = T)]

  # generate a vector of dataframe columns to keep from key file input to
  # header_name
  varsToKeep <- profileData %>%
    dplyr::select(header_name) %>%
    dplyr::pull()

  # pull targeted vars from each data frame based on header_names in key file
  googleDirData <- purrr::map(googleDirData, select, one_of(varsToKeep))

  # rename investigator names to key file names
  googleDirData <- lapply(googleDirData, function(frame) {
    setNames(frame, profileData$var[match(names(frame), profileData$header_name)]) })


  # add experiment and treatment units to data files

  # generate a vector of ALL POSSIBLE experiment and treatment var names from the
  # key file
  experimentTreatmentVarSet <- c(
    'L1',
    'L2',
    'L3',
    'L4',
    'L5',
    'L6',
    'tx_L1',
    'tx_L2',
    'tx_L3',
    'tx_L4',
    'tx_L5',
    'tx_L6'
  )

  if(profileData %>% filter(var %in% experimentTreatmentVarSet) %>% nrow() > 0) {

    googleDirData <- lapply(googleDirData,
                            add_exp_trt_levels,
                            profileData = profileData,
                            experimentTreatmentVarSet = experimentTreatmentVarSet)

  }


  # +++++++++++++++++++++++++++++++++++++++
  # BEGIN STANDARDIZE UNITS::profile data
  # source('~/Dropbox/development/standardize_units_profile.R')

  # profile tab DATA containing units only
  profileDataUnits <- profileData %>%
    dplyr::filter(!is.na(unit_levels)) %>%
    dplyr::select(header_name, unit_levels, Var_long, var)

  # join profile DATA with units and corresponding vars in conversion table
  PDU_UCP <- dplyr::left_join(profileDataUnits, unitsConversionProfile,
                              by = c("var", "unit_levels"),
                              suffix = c(".PD", ".UT")) %>%
    dplyr::filter(
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
            dplyr::filter(var == dataCol) %>%
            dplyr::select(var_notes)

          notes <- notes %>%
            dplyr::mutate(var_notes = replace(var_notes,
                                              var == dataCol,
                                              paste0(baseNote, "; UNITS CONVERSION APPLIED: ", PDU_UCP[PDU_UCP$var == dataCol,]['unitConversionFactor'])))
        } else {
          print(paste("add_row: ", dataCol))
          notes <- notes %>%
            tibble::add_row(source = 'profile',
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
    dplyr::select(var, Value) %>%
    tidyr::spread(key = var, value = Value)

  # merge location data with each data frame
  googleDirData <- lapply(googleDirData, function(frame) {
    merge(locationDataWide, frame, all = T) })

  # ADD THE DATA FILE NAME TO EACH DATA FILE

  # see the following resource for the attribute approach:
  # http://max2.ese.u-psud.fr/epc/conservation/Girondot/Publications/Blog_r/Entrees/2014/3/11_Get_the_list_element_name_within_lapply().html

  # 1. add the list object name (the data file name) as an attribute called ref
  # to each list item. If no attributes exist, add the ref attribute, else if
  # attributes already exist (should always be the case), append ref to existing
  # attrs.
  for (i in 1:length(googleDirData)) if (is.null(attributes(googleDirData[[i]]))) {attributes(googleDirData[[i]]) <- list(ref=names(googleDirData)[i])} else {attributes(googleDirData[[i]]) <- c(attributes(googleDirData[[i]]), ref=names(googleDirData)[i])}

  # 2. apply the ref attribute (the object name) as a column titled source_data;
  # move file identity details to the first few columns
  googleDirData <- lapply(googleDirData, function(x) {
    x %>% mutate(data_file = attributes(x)$ref) %>%
      select(google_dir, data_file, google_id, everything())
  })

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
