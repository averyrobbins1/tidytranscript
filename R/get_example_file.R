#' Get the file path to the example transcript within the package
#'
#' @return A file path
#' @export
#'
#' @examples
#' \dontrun{
#' get_example_file(dat)
#' }
get_example_file <- function() {
    system.file(
        "extdata",
        "example_transcript.pdf",
        package="tidytranscript")
}
