#' Get csv-s from folder
#'
#' Reads all csv-s from a local folder into one nested data frame
#' @name get_from_folder
#' @usage get_from_folder(local_path = NULL)
#' @param local_path A google drive path where the sheets are stored
#' @return The content of all spreadsheets in a local folder in one nested tibble
#' @examples
#' get_from_folder("Research/meta-analysis12/Extraction1/")
# TODO: error handling: check if files can be read as csv-s

get_from_folder <- function(local_path = NULL, all_char = TRUE){

    files <- list.files(local_path, full.names = TRUE)

    stopifnot(length(files) > 0,
              all(stringr::str_detect(files, ".csv$"))
              )

    # Try to guess the col_types
    if (all_char == FALSE) {
        tibble::tibble(file = files) %>%
        dplyr::mutate(sheet = purrr::map(file,
                                         ~readr::read_csv(.x)
                                         )
                      )
    }
    # All col_types will be character
    if (all_char == TRUE) {
        tibble::tibble(file = list.files(local_path,
                                         full.names = TRUE)) %>%
        dplyr::mutate(sheet = purrr::map(file,
                                         ~readr::read_csv(.x,
                                                           col_types = readr::cols(.default = "c")
                                                          )
                                         )
                      )
    }

}


