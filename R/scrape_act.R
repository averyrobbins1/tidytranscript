#' Scrape ACT scores from a student's transcript
#'
#' @param .data A character vector returned from the read_transcript function.
#'
#' @return A tibble of ACT test scores
#' @export
#'
#' @examples
#' scrape_act(dat)
scrape_act <- function(.data) {
    `%>%` <- magrittr::`%>%`

    len <- .data %>% purrr::simplify() %>% length()

    half1 <- .data %>%
        .[11:len] %>%
        stringr::str_sub(start = 1L, end = 57L)

    half2 <- .data %>%
        .[11:len] %>%
        stringr::str_sub(start = 58L)

    dat2 <- vctrs::vec_c(half1, half2)

    dat2 %>%
        tibble::enframe() %>%
        dplyr::mutate(test = stringr::str_detect(value, "\\bACT")) %>%
        dplyr::mutate(lag1 = dplyr::lag(test, n = 1),
               lag2= dplyr::lag(test, n = 2),
               test_data =
                   ifelse(test == TRUE | lag1 == TRUE | lag2 == TRUE,
                          TRUE, FALSE)) %>%
        dplyr::filter(test_data == TRUE) %>%
        dplyr::select(value) %>%
        dplyr::mutate(date = stringr::str_extract(value, '\\d{2}\\/\\d{2}\\/\\d{2}'),
               test = stringr::str_extract(value, 'ACT'),
               info = ifelse(is.na(date)|is.na(test),
                             value, NA)
        ) %>%
        tidyr::fill(date, test) %>%
        dplyr::filter(!is.na(info)) %>%
        dplyr::select(-value) %>%
        dplyr::mutate(info = stringr::str_trim(info) %>% stringr::str_squish()) %>%
        tidyr::separate(info, into = c('ENG', 'MATH', 'READ', 'SCI', 'COMP'), sep = " ") %>%
        dplyr::filter(ENG != "ENG") %>%
        tidyr::pivot_longer(cols = c(ENG, MATH, READ, SCI, COMP),
                     names_to = 'subject', values_to = 'score') %>%
        dplyr::mutate(date = lubridate::mdy(date)) %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::select(-date)
}
