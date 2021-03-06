% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape_birthday.R
\name{scrape_birthday}
\alias{scrape_birthday}
\title{Scrape a student's birthday from their transcript}
\usage{
scrape_birthday(.data, tibble = TRUE)
}
\arguments{
\item{.data}{A character vector returned from the read_transcript function.}

\item{tibble}{A logical, specifying if a tibble should be returned or not.}
}
\value{
A tibble by default, otherwise a double vector.
}
\description{
Scrape a student's birthday from their transcript
}
\examples{
\dontrun{
scrape_birthday(dat)
}
}
