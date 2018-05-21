#' @title Import a Google Sheet
#'
#' @description The sheet_download function downloads the contents of a Google
#'   Sheet into an R object with with optional parameters for rows to skip and a
#'   missing value code upon import.
#'
#' @note sheet_download is intended primarily as a helper function for batch
#'   downloading Google Sheets via a apply or map call but can be used
#'   independently.
#'
#' @param fileName name of the Google Sheet to import
#' @param skipRows number of rows to skip when importing (optional; default:0)
#' @param missingValueCode string to denote missing values (optional;
#'   default:'NA')
#'
#' @importFrom googlesheets gs_title
#' @importFrom googlesheets gs_read
#'
#' @return R object of type tibble
#'
#' @export
#'

sheet_download <- function(fileName, skipRows, missingValueCode) {

  if (missing(skipRows)) {
    skipRows <- 0
  }

  if (missing(missingValueCode)) {
    missingValueCode <- "NA"
  }

  token <- googlesheets::gs_title(fileName)

  dataFile <- googlesheets::gs_read(token,
                                    skip = skipRows,
                                    na = missingValueCode)

}
