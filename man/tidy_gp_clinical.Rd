% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sqlite_db.R
\name{tidy_gp_clinical}
\alias{tidy_gp_clinical}
\title{Tidy UK Biobank primary care clinical events}
\usage{
tidy_gp_clinical(gp_clinical, .details_only = FALSE)
}
\arguments{
\item{gp_clinical}{The UK Biobank primary care clinical events dataset}

\item{.details_only}{logical. If \code{TRUE}, return a character vector of
output table names only}
}
\value{
A named list. Item 'clinical_events' contains the read codes with
event dates, and item 'gp_clinical_values' contains the 'value' columns.
}
\description{
Reformats the UK Biobank primary care clinical events dataset to match the
output format for \code{\link{tidy_clinical_events}}.
}
\details{
The UK Biobank primary care clinical events data lists read codes in separate
columns, one for Read2 and one for Read3. This function reshapes the data to
long format so that all codes are in a single column. The \code{index} column
values relate to row numbers in the original data.

The primary care data also contains 3 'value' columns. The clinical
codes/dates in long format and 'value' columns are both returned in a list
under the names 'clinical_events' and 'gp_clinical_values' respectively.
}
\section{Other notes}{


By default, special date values (see
\href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=591}{resource 591} for
further details) are set to \code{NA}.
}

\seealso{
\code{\link{tidy_clinical_events}},
\code{\link{make_clinical_events_db}}
}
\keyword{internal}
