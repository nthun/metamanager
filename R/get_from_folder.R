#' Get csv-s from folder
#'
#' Reads all csv-s from a local folder into one nested data frame
#' @name get_from_folder
#' @usage get_from_folder(local_path = NULL, all_char = TRUE)
#' @param local_path A google drive path where the sheets are stored
#' @param all_char Read all varaibles as characters? (no data loss)
#' @return The content of all spreadsheets in a local folder in one nested tibble
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate tibble
#' @importFrom purrr map
#' @importFrom readr read_csv cols
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_from_folder("Research/meta-analysis12/Extraction1/")
#' }
# TODO: error handling: check if files can be read as csv-s

get_from_folder <- function(local_path = NULL,
                            all_char = TRUE){

    files <- list.files(local_path, full.names = TRUE)

    stopifnot(length(files) > 0,
              all(str_detect(files, ".csv$"))
              )

    # Try to guess the col_types
    if (all_char == FALSE) {
        tibble(file = files) %>%
        mutate(sheet = map(file, ~read_csv(.x)))
    }
    # All col_types will be character
    if (all_char == TRUE) {
        tibble(file = list.files(local_path,
                                         full.names = TRUE)) %>%
        mutate(sheet = map(file,
                           ~read_csv(.x, col_types = cols(.default = "c"))
                           )
                      )
    }

}


