% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_tests.R
\name{prepare_tests}
\alias{prepare_tests}
\title{Scrape standardized tests from a student's transcript}
\usage{
prepare_tests(.data)
}
\arguments{
\item{.data}{A character vector returned from the read_transcript function.}
}
\value{
A tibble with all of the test scores normalized
}
\description{
Scrape standardized tests from a student's transcript
}
\examples{
\dontrun{
prepare_tests('data_raw/my_transcript.pdf')
}
}
