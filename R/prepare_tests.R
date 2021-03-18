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
prepare_tests <- function(.data) {

    `%>%` <- magrittr::`%>%`

    # input for prepare_tests will first go through scrape_all_tests
    # is this poor design? possibly.
    tests <- tidytranscript::scrape_all_tests(.data)


    # return this tibble if the student doesn't have any tests
    # on their transcript
    if (nrow(tests) == 0) {
        return(
            tibble::tribble(
                ~test_composite,~test_english,~test_math,~test_science,
                NA_real_,      NA_real_,      NA_real_,  NA_real_

            )
        )
    }

    tests <- tests %>% dplyr::mutate(score = as.double(score))

    # custom min max function for ACT/SAT
    min_max <- function(x, min, max){
        (x - min) / (max - min)
    }

    if (purrr::has_element(tests$test, 'ACT')) {

        tests <- dplyr::filter(tests, test == 'ACT') %>%
            dplyr::mutate(score = min_max(score, 1, 36))

    } else if (purrr::has_element(tests$test, 'SATR')) {

        tests <- dplyr::filter(tests, test == 'SATR') %>%
            dplyr::mutate(
                score = dplyr::case_when(
                    subject == 'READING' |
                    subject == 'WRITING'   ~ min_max(score, 10, 40),
                    subject == 'COMPOSITE' ~ min_max(score, 400, 1600),
                    TRUE                   ~ min_max(score, 200, 800)
                )
            )
    } else if (purrr::has_element(tests$test, 'SAT')) {

        tests <- dplyr::filter(tests, test == 'SAT') %>%
            dplyr::mutate(
                score = dplyr::case_when(
                    subject %in% c('WRIT', 'MATH', 'CRIT_READ')
                                           ~ min_max(score, 200, 800),
                    subject == 'COMPOSITE' ~ min_max(score, 400, 1600),
                    TRUE                   ~ min_max(score, 200, 800)
                )
            )

    }

    english <- 'eng|english|read|reading|writ|write|writing'
    math <- 'math|mathematics'
    comp <- 'comp|composite'
    science <- 'sci|science'

    tests <- tests %>%
        dplyr::transmute(subject = stringr::str_to_lower(subject),
                         score) %>%
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

    tests <- tests %>%
        tidyr::pivot_wider(names_from = subject, values_from = score)

    if (length(names(tests)) < 3) {
        tests <- dplyr::mutate(tests, test_science = NA_real_) %>%
            dplyr::mutate(test_composite = NA_real_, .before = 'test_english')
    } else if (length(names(tests)) < 4) {
        tests <- dplyr::mutate(tests, test_science = NA_real_)
    }
    tests
}



