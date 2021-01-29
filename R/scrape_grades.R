#' Scrape a student's grades earned at BYUI from their transcript
#'
#' @param .data A character vector returned from the read_transcript function.
#'
#' @return A tibble with four resulting columns: course, credit, grade & semester
#' @export
#'
#' @examples
#' scrape_grades(dat)
scrape_grades <- function(.data) {

    `%>%` <- magrittr::`%>%`

    # funky t pipe
    # https://stackoverflow.com/questions/46239615/suppresswarnings-doesnt-work-with-pipe-operator

    `%T>%` <- magrittr::`%T>%`

    len <- length(.data)

    half1 <- .data %>%
        .[11:len] %>%
        stringr::str_sub(start = 1L, end = 57L)

    half2 <- .data %>%
        .[11:len] %>%
        stringr::str_sub(start = 58L)

    get_sem_year <- function(.data) {
        .data %>%
            tibble::enframe(name = 'row_num', value = 'text') %>%
            dplyr::mutate(sem = stringr::str_match(text, 'Winter|Spring|Fall|Summer|Transfer'),
                   year = stringr::str_match(text, '20\\d{2}'),
                   continued = stringr::str_detect(text, 'continued'),
                   cont = stringr::str_detect(text, '\\(cont\\.\\)'))
    }

    dat2 <- list(half1, half2) %>%
        purrr::map_dfr(get_sem_year) %>%
        dplyr::mutate(sem2 = sem) %>%
        tidyr::fill(sem2) %>%
        dplyr::mutate(sem = ifelse(sem2 == 'Transfer', sem2, sem)) %>%
        dplyr::select(-sem2) %>%
        dplyr::mutate(
            trans = ifelse(
                sem == 'Transfer' & continued == TRUE & cont == FALSE,
                'Transfer', NA)) %>%
        tidyr::fill(trans) %>%
        dplyr::mutate(
            sem = ifelse(continued == FALSE & cont == TRUE & trans == 'Transfer',
                         'Transfer', sem)) %>%
        dplyr::select(-trans, -continued, -cont) %>%
        tidyr::fill(sem, year)

    dat3 <- dat2 %>%
        dplyr::select(-row_num) %>%
        dplyr::mutate(text = stringr::str_trim(text)) %>%
        dplyr::filter(stringr::str_detect(text, '^\\w+\\s*\\d+'), # get rows that have one or more character
               # at the beginning of the pattern,
               # followed by zero or more spaces,
               # followed by 1 or more digits
               stringr::str_detect(text, '^ses', negate = TRUE), # remove rows that start w/ ses
               stringr::str_detect(text, '^cum', negate = TRUE), # remove rows that start w/ cum
               stringr::str_detect(text, '^res', negate = TRUE), # remove rows that start w/ res
               stringr::str_detect(text, '^\\d+', negate = TRUE) # remove rows that start w/ a digit,
               # (courses shouldn't start with a
               # a digit, right?)
        ) %>%
        tidyr::separate(text, into = c('text', 'grade'), sep = -2) %>% # the sep = -2 kind of
        # assumes that a grade
        # can't be > length 2
        # (A-,D+,WV,IP, etc.)
        dplyr::mutate(dplyr::across(tidyselect::everything(), stringr::str_trim)) %>%
        tidyr::separate(text, into = c('text', 'credit'), sep = -5) %T>%  # max credit length would be 5
        {options(warn = -1)} %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(), stringr::str_trim)) %>%
        dplyr::filter(sem != 'Transfer' | sem != NA)

    dat3 %>%
        dplyr::mutate(text = stringr::str_replace(text, '(?<=[A-Z])(?=[0-9])', ' ')) %>% # add a space in between
        # text and num part of
        # course code
        # follow the link below to see the inspiration for the regex above,
        # https://stackoverflow.com/questions/9756360/split-character-data-into-numbers-and-letters

        dplyr::mutate(text = stringr::str_replace(text, '\\s{2,}', ' ')) %>%
        tidyr::separate(text, into = c('course_text', 'course_num', 'text'), sep = '\\s') %>%
        dplyr::select(-text) %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(), stringr::str_trim)) %>%
        tidyr::unite(col = 'course', course_text, course_num, sep = ' ') %>%
        dplyr::mutate(semester = stringr::str_sub(sem, 1, 2) %>% stringr::str_to_upper(),
               year = stringr::str_sub(year,3,4)) %>%
        tidyr::unite(col = 'semester', semester, year, sep = '') %>%
        dplyr::mutate(semester = ifelse(semester == 'NANA', NA, semester)) %>%
        dplyr::select(-sem)
}
