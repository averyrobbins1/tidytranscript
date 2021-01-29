#' Scrape all desired data from a student's transcript
#'
#' @param file A path to a file (either a single string or a raw vector)
#'
#' @return A tibble with all of the data that we care about
#' @export
#'
#' @examples
#' tidytranscript('data_raw/my_transcript.pdf')
tidytranscript <- function(file) {

    dat <- tidytranscript::read_transcript(file)

    dplyr::mutate(
        tidytranscript::scrape_grades(dat),
        birthday = tidytranscript::scrape_birthday(dat, FALSE),
        major = tidytranscript::scrape_major(dat, FALSE),
        earned_credits = tidytranscript::scrape_earned_credits(dat, FALSE)
    )
}
