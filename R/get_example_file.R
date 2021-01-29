#' Get the file path to the example transcript within the package
#'
#' @return A file path
#' @export
#'
#' @examples
#' get_example_file()
get_example_file <- function() {
    tidytranscript::system.file(
        "extdata",
        "example_transcript.pdf",
        package="tidytranscript")
}
