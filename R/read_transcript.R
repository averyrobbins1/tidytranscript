#' Read a single PDF into a character vector
#'
#' @param file A path to a file (either a single string or a raw vector)
#'
#' @return A character vector
#' @export
#'
#' @examples
#' read_transcript('data/my_transcript.pdf')
read_transcript <- function(file) {
    readr::read_lines(pdftools::pdf_text(file))
}
