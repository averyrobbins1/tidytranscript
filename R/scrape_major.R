#' Scrape a student's major from their transcript
#'
#' @param .data A character vector returned from the read_transcript function.
#' @param tibble A logical, specifying if a tibble should be returned or not.
#'
#' @return A tibble by default, otherwise a character vector.
#' @export
#'
#' @examples
#' \dontrun{
#' scrape_major(dat)
#' }
scrape_major <- function(.data, tibble = TRUE) {
    `%>%` <- magrittr::`%>%`

    dat <- .data[stringr::str_detect(.data, 'Major')] %>%
        stringr::str_extract_all('.+(?=Minor)') %>%
        purrr::simplify() %>%
        stringr::str_trim() %>%
        stringr::str_remove('Major\\:') %>%
        stringr::str_trim()

    if (tibble) {
        tibble::as_tibble_col(dat, column_name = 'major')
    } else {
        dat
    }
}
