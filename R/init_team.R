#' Create a team file on google drive
#'
#' Create a template team file on google drive and open it in the browser
#' @name init_team
#' @usage init_team(gdrive_path = NULL, file_name = "meta_team", browse = TRUE)
#' @param gdrive_path a full (new) gdrive path <chr>, preferably with / at the end
#' @param file_name a vector <chr> of folder names to create in path
#' @param browse should the team file open in the browser? <lgl>
#' @return no output, this function exerts a side-effect
#' @examples
#' # Creating default folder structure
#' \dontrun{
#' init_team("research/meta", "My meta-analysis team")
#' # Create with default name and without editing in browser
#' init_team("research/meta", browse = FALSE)
#' }

init_team <- function(gdrive_path = NULL, file_name = "meta_team", browse = TRUE){

    stopifnot(length(gdrive_path) == 1,
              length(name) == 1)

    # Run listing safely, so if fails, does not stop the function
    safe_drive_ls <- purrr::safely(googledrive::drive_ls)
    drive_list <- safe_drive_ls(gdrive_path)

    # Stop if the path does not exist, or there is a file with the same name
    stopifnot(nrow(drive_list$result) > 0,
              nrow(dplyr::filter(drive_list$result, name == file_name)) == 0)

    # Create template table and save to
    tibble::tibble(name = NA_character_,
                   role = NA_character_,
                   screening_effort = NA_real_,
                   extraction_effort = NA_real_) %>%
    readr::write_excel_csv(path = paste0(tempdir(), "/team_df.csv"), na = "")


    team_file <-
        googledrive::drive_upload(
                                media = paste0(tempdir(), "/team_df.csv"),
                                name = name,
                                path = gdrive_path,
                                type = "spreadsheet"
        )

    if (browse == TRUE){
        googlesheets::gs_key(team_file$id) %>%
        googlesheets::gs_browse()
    }
}
