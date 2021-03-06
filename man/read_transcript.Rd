% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_transcript.R
\name{read_transcript}
\alias{read_transcript}
\title{Read a single PDF into a character vector}
\usage{
read_transcript(file)
}
\arguments{
\item{file}{A path to a file (either a single string or a raw vector)}
}
\value{
A character vector
}
\description{
Read a single PDF into a character vector
}
\examples{
\dontrun{
dat <- read_transcript('data/my_transcript.pdf')
}
}
