#' Scrape all available test scores from a student's transcript
#'
#' @param .data A character vector returned from the read_transcript function.
#'
#' @return A tibble of available test scores
#' @export
#'
#' @examples
#' scrape_all_tests(dat)
scrape_all_tests <- function(.data) {
    `%>%` <- magrittr::`%>%`

    has_test <- function(.data, test) {
        .data %>%
            tibble::enframe() %>%
            dplyr::mutate(has_test = stringr::str_detect(value, test)) %>%
            dplyr::pull(has_test) %>%
            purrr::has_element(TRUE)
    }

    if (has_test(.data, '\\bSAT$')) {
        dat1 <- tidytranscript::scrape_sat(.data)
    } else {
        dat1 <- tibble::tibble()
        print("You don't have SAT scores!")
    }

    if (has_test(.data, '\\bACT')) {
        dat2 <- tidytranscript::scrape_act(.data)
    } else {
        dat2 <- tibble::tibble()
        print("You don't have ACT scores!")
    }

    if (has_test(.data, '\\bSATR')) {
        dat3 <- tidytranscript::scrape_satr(.data)
    } else {
        dat3 <- tibble::tibble()
        print("You don't have SATR scores!")
    }

    dplyr::bind_rows(dat1, dat2, dat3)
}
