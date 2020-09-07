#' @title Homogenize LTER Soil Organic Matter Working Group data and notes
#'
#' @description The function data_homogonization imports data and a key
#'   translation file from a Google Directory that the user is able to access.
#'   Data are homogenized to feature common header names and units, and
#'   dataset-level notes appended to each data file. Output includes
#'   standardized data and notes to a local directory on the user's computer
#'   that are also uploaded to the Google Directory identified by the user as
#'   containing the source data and key translation file. Modified data are
#'   appeneded with the string "HMGZD".
#'
#' @note data_homogonization relies on the helper function sheet_download.
#'
#' @param directoryName The URL of the Google Drive directory containing data
#'   and a key translation file
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
#' @import tools
#' @importFrom googlesheets4 read_sheet
#' @importFrom stringr str_extract
#' @importFrom purrr map_df
#' @import pander
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


  # check for required input to function ------------------------------------

  # directoryName
  if (missing(directoryName)) {
    stop("provide the name of a Google Drive directory")
  }

  # temporaryDirectory
  if (missing(temporaryDirectory)) {
    stop("provide the name of a local directory to house script output")
  }


  # local output directory --------------------------------------------------

  # a user-identified temporaryDirectory is required
  # ensure the provided temporaryDirectory has a trailing slash
  if (stringr::str_extract(temporaryDirectory, ".$") != "/") {
    temporaryDirectory <- paste0(temporaryDirectory, "/")
  }


  # create the receiving directory if it does not exist; delete the contents if
  # it does exist
  if (!dir.exists(temporaryDirectory)) {

    dir.create(temporaryDirectory)

  } else {

    file.remove(file.path(temporaryDirectory, list.files(temporaryDirectory)))

  }


  # access Google Drive directory -------------------------------------------

  if (grepl("https://", directoryName)) {

    downloadName <- directoryName
    directoryName <- drive_get(directoryName)$name

  } else {

    downloadName <- directoryName

  }

  # access Google directory id for reference
  googleID <- googledrive::drive_get(downloadName) %>%
    dplyr::pull(id)

  # list files in Google directory
  dirFileList <- googledrive::drive_ls(path = downloadName)

  # isolate names from Google directory
  dirFileNames <- dirFileList %>%
    filter(
      !grepl("duplicates", name), # remove duplicates file/folder
      file_ext(name) != "zip", # remove zipped files
      file_ext(name) != "pdf", # remove PDF files
      file_ext(name) != "html", # remove html files
      file_ext(name) != "txt", # remove txt files
      file_ext(name) != "R" # remove R files
    ) %>%
    select(name) %>%
    pull(name)


  # access and process key file ---------------------------------------------

  # Update 2018-12-28: data_harmonization requires a key file version 2
  if (!any(grepl("KEY_RC", dirFileNames, ignore.case = F))) {

    stop("data_harmonization requires a key file version RC")

  }

  # isolate key file, and extract details in location and profile tabs
  keyFileName <- grep("KEY_RC", dirFileNames, ignore.case = F, value = T)
  keyFileToken <- dirFileList[dirFileList$name == keyFileName, ]$id

  # extract location and profile tabs of key file
  locationData <- googlesheets4::read_sheet(
    keyFileToken,
    sheet = 1,
    col_types = "c")

  profileData <- googlesheets4::read_sheet(
    keyFileToken,
    sheet = 2) %>%
  mutate_all(as.character)


  # key file location tab QC ------------------------------------------------

  # (1) confirm requisite input to location tab
  locationRequiredFields <- c(
    "curator_PersonName",
    "curator_organization",
    "curator_email",
    "time_series",
    "gradient",
    "experiments",
    "merge_align"
  )

  if (any(is.na(locationData[locationData[["var"]] %in% locationRequiredFields,]["Value"]))) {

    stop("at least one required field in location tab is missing (see output for missing (NA) value)")
    print(locationData[locationData[["var"]] %in% locationRequiredFields,][c("var", "Value")])

  }

  # remove missing fields and add Google reference details to location
  locationData <- locationData %>%
    dplyr::filter(!is.na(Value)) %>%
    tibble::add_row(Value = googleID, var = "google_id") %>%
    tibble::add_row(Value = directoryName, var = "google_dir")

  # remove missing fields from profile
  profileData <- profileData %>%
    dplyr::filter(!is.na(header_name))


  # generate note file from key file input ----------------------------------

  # capture notes from key file location and profile tabs
  notes <- dplyr::bind_rows(
    tibble(
      source = "location",
      Var_long = "Google Directory",
      var = NA,
      var_notes = directoryName
    ),
    locationData %>%
      filter(var %in% c("network", "site_code", "location_name")) %>%
      dplyr::mutate(source = "location") %>%
      dplyr::select(source, Var_long, var, var_notes = Value),
    locationData %>%
      dplyr::filter(!is.na(var_notes)) %>%
      dplyr::mutate(source = "location") %>%
      dplyr::select(source, Var_long, var, var_notes),
    profileData %>%
      dplyr::filter(!is.na(Notes) | !is.na(Comment)) %>%
      mutate(var_notes = case_when(
        !is.na(Notes) & !is.na(Comment) ~ paste0(Notes, "; ", Comment),
        !is.na(Notes) & is.na(Comment) ~ Notes,
        is.na(Notes) & !is.na(Comment) ~ Comment)
      ) %>%
      dplyr::mutate(source = "profile") %>%
      dplyr::select(source, Var_long, var, var_notes)
  )


  # empty tibble to house unit conversion notes -----------------------------

  conversionNotes <- tibble(
    source = as.character(),
    var = as.character(),
    Var_long = as.character(),
    given_unit = as.character(),
    target_unit = as.character(),
    unit_conversion_factor = as.numeric(),
    varNotes = "converted"
  )


  # begin standardize units::location data ----------------------------------

  # location tab DATA containing units only
  locationDataUnits <- locationData %>%
    dplyr::filter(!is.na(Unit)) %>%
    dplyr::select(Value, unit_levels = Unit, Var_long, var)

  # join location DATA with units and corresponding vars in conversion table
  LDU_UCL <- dplyr::left_join(locationDataUnits, unitsConversions,
                              by = c("var", "unit_levels"),
                              suffix = c(".PD", ".UT")) %>%
    dplyr::filter(
      !is.na(unitConversionFactor),
      unitConversionFactor != 1
    )

  # standardize location data units
  for (varValue in c(LDU_UCL$var)) {

    # standardize values per the units_conversion_table
    locationData[locationData$var == varValue,]["Value"] <- as.numeric(locationData[locationData$var == varValue,]["Value"]) * LDU_UCL[LDU_UCL$var == varValue,]["unitConversionFactor"]

    conversionNotes <- conversionNotes %>%
      add_row(source = "location",
              var = varValue,
              Var_long = LDU_UCL[LDU_UCL$var == varValue,]$Var_long.PD,
              given_unit = LDU_UCL[LDU_UCL$var == varValue,]$givenUnit,
              target_unit = LDU_UCL[LDU_UCL$var == varValue,]$unit_levels,
              unit_conversion_factor = LDU_UCL[LDU_UCL$var == varValue,]$unitConversionFactor,
              varNotes = 'converted'
      )

  } # close standardize location data units

  # end standardize units::location data
  # +++++++++++++++++++++++++++++++++++++++


  # location data QC --------------------------------------------------------

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

    if (targetValue[["class"]] == "numeric") {

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

    } else if (targetValue[["class"]] == "character") {

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
  location_range_report <- map_df(.x = locationQC %>% filter(!is.na(minValue)) %>% inner_join(locationData, by = c("var")) %>% filter(!is.na(Value)) %>% pull(var),
                                  .f = location_range_check)
  location_type_report <- map_df(.x = locationQC %>% filter(!is.na(class)) %>% inner_join(locationData, by = c("var")) %>% filter(!is.na(Value)) %>% pull(var),
                                 .f = location_type_check)

  location_QC_report <- bind_rows(location_range_report, location_type_report)

  # report if location QC errors detected
  if (nrow(location_QC_report) > 0) {

    location_QC_report <- location_QC_report %>%
      group_by(var, error) %>%
      distinct() %>%
      mutate(
        dataset = directoryName,
        source = "location"
      ) %>%
      select(dataset, source, var, error)

    warning("location QC errors dectected, see NOTES file")
    print(location_QC_report)

  }

  # end qc check::location data
  # +++++++++++++++++++++++++++++++++++++++


  # import data file(s) -----------------------------------------------------

  # set import parameters

  # Isolate rows to skip from locationData for data import. This was originally
  # intended to be an input as to the number of rows to skip but it seems to
  # have been interpreted by users as the row number of the header.
  if (length(locationData[locationData$var == "header_row",]$var) == 1) {

    skipRows = as.numeric(locationData[locationData$var == "header_row",]$Value) - 1

  } else {

    skipRows = 0

  }

  # isolate missing value codes from locationData for data import
  if (length(locationData[locationData$var == "NA_1",]$var) == 1) {

    mvc1 = locationData[locationData$var == "NA_1",]$Value }

  if (length(locationData[locationData$var == "NA_2",]$var) == 1) {

    mvc2 = locationData[locationData$var == "NA_2",]$Value }

  missingValueCode = "NA"
  if (exists("mvc1")) { missingValueCode = mvc1 }
  if (exists("mvc2")) { missingValueCode = mvc2 }
  if (exists("mvc1") && exists("mvc2")) { missingValueCode = c(mvc1, mvc2) }

  # import all (data + key) files from google dir
  googleDirData <- lapply(dirFileList[dirFileList$name %in% dirFileNames, ]$id,
    sheet_download,
    missingValueCode = missingValueCode,
    skipRows = skipRows)

  # add filenames
  names(googleDirData) <- dirFileNames

  # as key file is already loaded, remove it from the list of data frames
  # googleDirData <- googleDirData[-grepl("key", names(googleDirData),
  # ignore.case = T)]
  googleDirData <- googleDirData[!grepl("key", names(googleDirData), ignore.case = T)]

  # generate a vector of dataframe columns to keep from key file input to
  # header_name
  varsToKeep <- profileData %>%
    dplyr::select(header_name) %>%
    dplyr::pull()

  # pull targeted vars from each data frame based on header_names in key file
  suppressWarnings(
    googleDirData <- purrr::map(googleDirData, select, one_of(varsToKeep))
  )

  # rename investigator names to key file names
  googleDirData <- lapply(googleDirData, function(frame) {
    setNames(frame, profileData$var[match(names(frame), profileData$header_name)]) })


  # add experiment and treatment units to data files

  # generate a vector of ALL POSSIBLE experiment and treatment var names from
  # the key file
  experimentTreatmentVarSet <- c(
    "L1",
    "L2",
    "L3",
    "L4",
    "L5",
    "L6",
    "tx_L1",
    "tx_L2",
    "tx_L3",
    "tx_L4",
    "tx_L5",
    "tx_L6"
  )

  if (profileData %>% filter(var %in% experimentTreatmentVarSet) %>% nrow() > 0) {

    googleDirData <- lapply(googleDirData,
      add_exp_trt_levels,
      profileData = profileData,
      experimentTreatmentVarSet = experimentTreatmentVarSet)

  }


  # begin standardize units::profile data -----------------------------------

  # profile tab DATA containing units only
  profileDataUnits <- profileData %>%
    dplyr::filter(!is.na(unit_levels)) %>%
    dplyr::select(header_name, unit_levels, Var_long, var)

  # join profile DATA with units and corresponding vars in conversion table
  PDU_UCP <- dplyr::left_join(profileDataUnits, unitsConversions,
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
        print(paste0("Start ", i))
        print(dataCol)  ###DEBUG FOR NEW KEY VAR
        print(str(googleDirData[[i]][[dataCol]]))  ###DEBUG FOR NEW KEY VAR
        print(PDU_UCP[PDU_UCP$var == dataCol, ][["unitConversionFactor"]])  ###DEBUG FOR NEW KEY VAR
        print(paste0("End ", i))
        
        
        googleDirData[[i]][[dataCol]] <- googleDirData[[i]][[dataCol]] * PDU_UCP[PDU_UCP$var == dataCol, ][["unitConversionFactor"]]

        conversionNotes <- conversionNotes %>%
          add_row(source = "profile",
                  var = dataCol,
                  Var_long = PDU_UCP[PDU_UCP$var == dataCol, ]$Var_long.PD,
                  given_unit = PDU_UCP[PDU_UCP$var == dataCol, ]$givenUnit,
                  target_unit = PDU_UCP[PDU_UCP$var == dataCol, ]$unit_levels,
                  unit_conversion_factor = PDU_UCP[PDU_UCP$var == dataCol, ]$unitConversionFactor,
                  varNotes = "converted"
          )


      } # close add mention of conversions to notes

    } # close loop through PDU_UCP

  } # close loop through googleDirData

  # END STANDARDIZE UNITS::profile data
  # +++++++++++++++++++++++++++++++++++++++


  # profile data QC ---------------------------------------------------------

  # isolate profile data vars where a range to check has been identified by
  # Will and Derek
  profileRanges <- profileQC %>%
    filter(!is.na(minValue)) %>%
    pull(var)

  profile_range_check <- function(frame) {

    profileRangeData <- frame %>%
      select(one_of(profileRanges))

    if (ncol(profileRangeData) > 0) {

      profileRangeData %>%
        summarise_all(funs(min), na.rm = TRUE) %>%
        gather(key = "var", value = "min") %>%
        inner_join(
          frame %>%
            select(one_of(profileRanges)) %>%
            summarise_all(funs(max), na.rm = TRUE) %>%
            gather(key = "var", value = "max"),
          by = c("var")
        ) %>%
        inner_join(
          profileQC %>%
            select(var, minValue, maxValue),
          by = c("var")
        ) %>%
        mutate(
          min = as.numeric(min),
          max = as.numeric(max)
        ) %>%
        filter(min < minValue | max > maxValue) %>%
        mutate(error = "out of range") %>%
        select(var, min, max, minValue, maxValue, error)

    } # close IF profile range vars to test are present

  } # close profile_range_check

  suppressWarnings(
    profile_range_report <- map_df(.x = googleDirData,
                                   .f = profile_range_check)
  )

  # CHECK TYPE

  # vector of all possible numeric vectors from QC template
  numericProfileVars <- profileQC %>%
    filter(class == "numeric") %>%
    pull(var)

  profile_type_check <- function(frame) {

    # data frame cols expected to be numeric
    expectedNumeric <- frame %>%
      select(one_of(numericProfileVars)) %>%
      select_if(Negate(is.numeric))

    # build tibble of non-numeric cols expected to be numeric
    tibble(
      var = colnames(expectedNumeric),
      error = "expected numeric"
    )

  }

  suppressWarnings(
    profile_type_report <- map_df(googleDirData, profile_type_check)
  )

  profile_QC_report <- bind_rows(profile_range_report, profile_type_report)

  # report if profile QC errors detected
  if (nrow(profile_QC_report) > 0) {

    profile_QC_report <- profile_QC_report  %>%
      mutate(
        dataset = directoryName,
        source = "profile"
      ) %>%
      select(dataset, source, var, error, min, min_allowed = minValue, max, max_allowed = maxValue)

    warning("profile errors detected, see NOTES file")
    print(profile_QC_report)

  }

  # END QC CHECK::profile data
  # +++++++++++++++++++++++++++++++++++++++


  # add location metadata and generate HMGZD filenames ---------------------

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
  for (i in 1:length(googleDirData)) if (is.null(attributes(googleDirData[[i]]))) {

    attributes(googleDirData[[i]]) <- list(ref = names(googleDirData)[i])

  } else {

    attributes(googleDirData[[i]]) <- c(attributes(googleDirData[[i]]), ref = names(googleDirData)[i])

  }

  # 2. apply the ref attribute (the object name) as a column titled source_data;
  # move file identity details to the first few columns
  googleDirData <- lapply(googleDirData, function(x) {
    x %>% mutate(data_file = attributes(x)$ref) %>%
      select(google_dir, data_file, google_id, everything())
  })

  # store names of data files before processing
  oeDataNames <- names(googleDirData)

  # rename files to include base name + indication of homogenization
  names(googleDirData) <- paste0(stringr::str_extract(names(googleDirData), "^[^\\.]*"), "_HMGZD")


  # expanded conversionNotes with vars not converted ------------------------

  # call the vars_not_converted function to document variables that are
  # candidates for conversion but were not converted. Include this output (if
  # any) with documentation regarding variables that were converted (i.e.,
  # append to conversionNotes).

  locationVarsNotConverted <- vars_not_converted(varType = "location",
                                                 unitsSource = locationDataUnits,
                                                 joinedUnits = LDU_UCL,
                                                 unitsConversionReference = unitsConversions)

  profileVarsNotConverted <- vars_not_converted(varType = "profile",
                                                unitsSource = profileDataUnits,
                                                joinedUnits = PDU_UCP,
                                                unitsConversionReference = unitsConversions)

  conversionNotes <- bind_rows(
    conversionNotes,
    locationVarsNotConverted,
    profileVarsNotConverted
  ) %>%
    rename(factor = unit_conversion_factor)

  # output to screen

  if (nrow(locationVarsNotConverted) > 0 | nrow(profileVarsNotConverted) > 0) {

    warning("some units not standaridized, see NOTES file")

    if (nrow(locationVarsNotConverted) > 0) {

      print(locationVarsNotConverted)

    }

    if (nrow(profileVarsNotConverted) > 0) {

      print(profileVarsNotConverted)

    }

  }

  # generate and upload QC report -------------------------------------------

  rmarkdown::render(input = system.file("homogenization_notes.Rmd", package = "soilHarmonization"),
                    params = list(
                      param_directory = directoryName,
                      param_namesOE = sort(oeDataNames),
                      param_namesHmgzd = sort(names(googleDirData)),
                      param_notes = notes,
                      param_conversion = conversionNotes,
                      param_locationQC = location_QC_report,
                      param_profileRange = profile_range_report,
                      param_profileType = profile_type_report
                    ),
                    output_file = paste0(temporaryDirectory, directoryName, "_HMGZD_NOTES.pdf"))

  googledrive::drive_upload(paste0(temporaryDirectory, directoryName, "_HMGZD_NOTES.pdf"),
                            path = googledrive::drive_get(googledrive::as_id(googleID)),
                            type = "application/pdf")


  # write data to file and upload to Google Drive ---------------------------

  # data to temporary location
  googleDirData %>%
    names(.) %>%
    purrr::map(~ readr::write_csv(googleDirData[[.]], paste0(temporaryDirectory, ., ".csv")))

  # identify directory with files (not full.names=T)
  filesToUpload <- list.files(path = temporaryDirectory,
                              pattern = "\\.csv$",
                              full.names = FALSE,
                              recursive = FALSE)

  # upload these files to the target Google directory
  lapply(filesToUpload, function(frame) {
    googledrive::drive_upload(paste0(temporaryDirectory, frame),
                              path = googledrive::drive_get(googledrive::as_id(googleID)),
                              type = "spreadsheet")
  })

}
