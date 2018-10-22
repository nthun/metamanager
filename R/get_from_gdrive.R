#' Get sheets from google drive
#'
#' Reads all google sheets from a google drive path into one nested data frame
#' Authentication in handled by the googledrive package
#' @name get_from_gdrive
#' @usage get_from_gdrive(gdrive_path = NULL, all_char = TRUE)
#' @param gdrive_path A google drive path where the sheets are stored
#' @param all_char Read all varaibles as characters? (no data loss)
#' @return The content of all spreadsheets in the google drive folder in one nested tibble
#' @importFrom purrr safely map
#' @importFrom dplyr transmute
#' @importFrom googlesheets gs_key gs_read
#' @importFrom googledrive drive_ls
#' @importFrom readr cols
#' @export
#'
#' @examples
#' \dontrun{
#' get_from_gdrive("Research/meta-analysis12/Extraction1/")
#' }
# TODO: error handling: check if all files are google sheets
# TODO: speed up by decreasing the scope of the search somehow

get_from_gdrive <- function(gdrive_path = NULL,
                            all_char = TRUE){

    # Run listing safely, so if fails, does not stop the function
    safe_drive_ls <- safely(googledrive::drive_ls)
    drive_list <- safe_drive_ls(gdrive_path)

    # If there is an error, stop
    if (!is.null(drive_list$error)) stop("The specified google drive path does not exist")

    # Try to guess the col_types
    if (all_char == FALSE) {
        transmute(drive_list$result,
                         file = name,
                         # Try to guess the col_types
                         sheet = map(id,
                                        ~gs_key(.x) %>%
                                         gs_read(1)
                                    )
                         )
    }
    if (all_char == TRUE) {
        # All col_types are read as character
        transmute(drive_list$result,
                  file = name,
                  sheet = purrr::map(id,
                                    ~gs_key(.x) %>%
                                     gs_read(1,
                                             col_types = readr::cols(.default = "c")
                                                                  )
                                            )
                         )
        }
}


