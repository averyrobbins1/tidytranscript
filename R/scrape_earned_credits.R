#' Scrape a student's credits earned while at BYU-I from their transcript
#' Does not work currently, do not use
#'
#' @param .data A character vector returned from the read_transcript function.
#' @param tibble A logical, specifying if a tibble should be returned or not.
#'
#' @return A tibble by default, otherwise a double vector.
#' @export
#'
#' @examples
#' \dontrun{
#' scrape_earned_credits(dat)
#' }
scrape_earned_credits <- function(.data, tibble = TRUE) {
    `%>%` <- magrittr::`%>%`

    len <- .data %>% purrr::simplify() %>% length()

    half1 <- .data %>%
        .[11:len] %>%
        stringr::str_sub(start = 1L, end = 57L)

    half2 <- .data %>%
        .[11:len] %>%
        stringr::str_sub(start = 58L)

    combined_vector <- vctrs::vec_c(half1, half2)

    dat <- combined_vector %>%
        tibble::enframe(name = 'row_num', value = 'text') %>%
        dplyr::mutate(end = stringr::str_match(text, 'End of Transcript')) %>%
        tidyr::fill(end, .direction = "down") %>%
        dplyr::filter(end == 'End of Transcript' & stringr::str_detect(text, 'cum')) %>%
        dplyr::slice(1) %>%
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

# old, may use later

# pdf <- fs::dir_ls('../sp_praxis/data_raw/pdf examples')
#
# dat <- read_transcript(pdf[5])
#
# temp <- function(dat) {
#     len <- dat %>% purrr::simplify() %>% length()
#
#     half1 <- dat %>%
#         .[11:len] %>%
#         stringr::str_sub(start = 1L, end = 57L)
#
#     half2 <- dat %>%
#         .[11:len] %>%
#         stringr::str_sub(start = 58L)
#
#     combined_vector <- vctrs::vec_c(half1, half2)
#
#     combined_vector
#
#     combined_vector %>%
#         enframe(name = NULL, value = 'text') %>%
#         mutate(text = str_trim(text)) %>%
#         filter(str_detect(text, 'Semester|Session|^cum')) %>%
#         mutate(sem = str_detect(text, 'Semester|Session'),
#                sem = ifelse(sem == TRUE, text, NA),
#                sem = str_remove_all(sem, '-') %>%
#                    str_remove(' Semester|Session') %>%
#                    str_trim()
#         ) %>%
#         fill(sem, .direction = 'down') %>%
#         filter(str_detect(text, 'cum'),
#                !is.na(sem)) %>%
#         mutate(text = str_trim(text) %>%
#                    str_replace_all('\\s+', '_')) %>%
#         separate(col = text,
#                  into = c('cum', 'attempt', 'earn', 'pass', 'quality', 'points', 'gpa'),
#                  sep = '_') %>%
#         mutate(sem = str_remove(sem, '\\s\\(cont\\.\\)')) %>%
#         select(earn, sem) %>%
#         distinct()
# }
#
# # map(pdf, ~ read_transcript(.x) %>% temp())
