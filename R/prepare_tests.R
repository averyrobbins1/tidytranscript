#' Scrape standardized tests from a student's transcript
#'
#' @param .data A character vector returned from the read_transcript function.
#'
#' @return A tibble with all of the test scores normalized
#' @export
#'
#' @examples
#' \dontrun{
#' prepare_tests('data_raw/my_transcript.pdf')
#' }
prepare_tests <- function(.data, normalize = FALSE) {

    `%>%` <- magrittr::`%>%`

    tests <- scrape_all_tests(.data)

    if (nrow(tests) == 0) {
        return(
            tibble::tribble(
                ~test_composite,~test_english,~test_math,~test_science,
                NA_real_,      NA_real_,      NA_real_,  NA_real_

            )
        )
    }

    if (purrr::has_element(tests$test, 'ACT')) {
        tests <- dplyr::filter(tests, test == 'ACT')
    } else if (purrr::has_element(tests$test, 'SAT')) {
        tests <- dplyr::filter(tests, test == 'SAT')
    } else if (purrr::has_element(tests$test, 'SATR')) {
        tests <- dplyr::filter(tests, test == 'SATR')
    }

    english <- 'eng|english|read|reading|write|writing'
    math <- 'math|mathematics'
    comp <- 'comp|composite'
    science <- 'sci|science'

    tests <- tests %>%
        dplyr::transmute(subject = stringr::str_to_lower(subject),
                         score = as.double(score)) %>%
        dplyr::mutate(
            subject = as.character(subject),
            subject = dplyr::case_when(
                stringr::str_detect(subject, english) ~ 'test_english',
                stringr::str_detect(subject, math)    ~ 'test_math',
                stringr::str_detect(subject, comp)    ~ 'test_composite',
                stringr::str_detect(subject, science) ~ 'test_science',
                TRUE                                  ~ 'error')) %>%
        dplyr::group_by(subject) %>%
        dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup()

    if (normalize) {
        tests <- tests %>%
            recipes::recipe() %>%
            recipes::step_normalize(score) %>%
            recipes::prep() %>%
            recipes::bake(new_data = NULL)
    }

    tests <- tests %>%
        tidyr::pivot_wider(names_from = subject, values_from = score)

    if (length(names(tests)) < 4) {
        tests <- mutate(tests, test_science = NA_real_)
    }

    tests
}



