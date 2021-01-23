#' Scrape a student's credits earned while at BYU-I from their transcript
#'
#' @param data A character vector returned from the read_transcript function.
#' @param tibble A logical, specifying if a tibble should be returned or not.
#'
#' @return A tibble by default, otherwise a double vector.
#' @export
#'
#' @examples
#' scrape_earned_credits(dat)
scrape_earned_credits <- function(.data, tibble = TRUE) {
    `%>%` <- magrittr::`%>%`

    dat <- .data %>%
        tibble::enframe(name = 'row_num', value = 'text') %>%
        dplyr::mutate(end = stringr::str_match(text, 'End of Transcript')) %>%
        tidyr::fill(end, .direction = "down") %>%
        dplyr::filter(end == 'End of Transcript' & stringr::str_detect(text, 'cum')) %>%
        dplyr::select(-row_num, -end) %>%
        dplyr::mutate(earned = stringr::str_remove(text, '\\(?[0-9,.]+\\)?') %>%
                          stringr::str_extract('\\(?[0-9,.]+\\)?') %>%
                          as.double()) %>%
        dplyr::pull(earned)

    if (tibble) {
        tibble::as_tibble_col(dat, column_name = 'earned_credits')
    } else {
        dat
    }
}
