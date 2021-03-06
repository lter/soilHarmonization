% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vars_not_converted.R
\name{vars_not_converted}
\alias{vars_not_converted}
\title{Catalog location- and profile-data variables that are candidates for
units conversion but for which a units conversion was not applied.}
\usage{
vars_not_converted(varType, unitsSource, joinedUnits, unitsConversionReference)
}
\arguments{
\item{varType}{The type of variable ("location", "profile") passed as a
string to be documented.}
}
\value{
A tibble or data frame object that is merged with other conversion
notes, and included in the homogenization report file (PDf).
}
\description{
The function vars_not_converted catalogs location- and
profile-data variables that are candidates for units conversion but for
which a units conversion was not applied. vars_not_converted is a helper
function that is called within data_homogenization(). Output is bound to
the convesionNotes objet, generated in data_homogenization() and documented
in the homogenization notes file (PDF).
}
\examples{
\dontrun{

 vars_not_converted("location"),
 vars_not_converted("profile")

 bind_rows(
   conversionNotes,
   vars_not_converted("location"),
   vars_not_converted("profile")
  )

}

}
