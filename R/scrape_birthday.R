#' Scrape a student's birthday from their transcript
#'
#' @param data A character vector returned from the read_transcript function.
#' @param tibble A logical, specifying if a tibble should be returned or not.
#'
#' @return A tibble by default, otherwise a double vector.
#' @export
#'
#' @examples
#' scrape_birthday(dat)
scrape_birthday <- function(.data, tibble = TRUE) {
    `%>%` <- magrittr::`%>%`

    dat <- .data[3] %>%
        stringr::str_extract_all('\\d{2}') %>%
        purrr::simplify() %>%
        stringr::str_c(collapse = '-') %>%
        lubridate::mdy()

    if (tibble) {
        tibble::as_tibble_col(dat, column_name = 'birthday')
    } else {
        dat
    }
}
