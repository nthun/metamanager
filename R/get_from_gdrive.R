#' Get sheets from google drive
#'
#' A generic function to get all spreadsheets in a gdrive and collapse them into one object
#' @name get_from_gdrive
#' @usage get_from_gdrive(gdrive_path)
#' @param gdrive_path A google drive path where the sheets are stored
#' @return The content of all spreadsheets in the google drive folder in one nested tibble
#' @examples
#' get_from_gdrive("Research/meta-analysis12/Extraction1/")
# TODO: error handling: check if all files are google sheets
# TODO: data validation, e.g. check if all the files have the right format

get_from_gdrive <- function(gdrive_path, all_char = TRUE){

    # Run listing safely, so if fails, does not stop the function
    safe_drive_ls <- purrr::safely(googledrive::drive_ls)
    drive_list <- safe_drive_ls(gdrive_path)

    # If there is an error, throw a warning
    if (!is.null(drive_list$error)) warning("The specified google drive inventory does not exist")

    # If there are no errors in the listing, download the files
    if (is.null(drive_list$error)){
        if (all_char == FALSE){
            drive_list$result %>%
                dplyr::transmute(file = name,
                                 # Try to guess the col_types
                                 sheet = purrr::map(id, ~googlesheets::gs_key(.x) %>%
                                                         googlesheets::gs_read(1)))
        } else {
            drive_list$result %>%
                dplyr::transmute(file = name,
                                 # All col_types are read as character
                                 sheet = purrr::map(id, ~googlesheets::gs_key(.x) %>%
                                                         googlesheets::gs_read(1,
                                                                               col_types = readr::cols(.default = "c"))))

        }
    }
}


