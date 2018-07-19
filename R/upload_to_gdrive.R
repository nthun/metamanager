#' Upload all .csv-s as google sheets from a local folder to a google drive folder
#'
#' The function reads all files from a google drive folder and copies the files to a gdrive folder
#' @name upload_to_gdrive
#' @usage upload_to_gdrive(local_path = NULL, gdrive_path = NULL, overwrite = FALSE)
#' @param local_path a full path <chr> that contains .csv-s to upload to gdrive
#' @param gdrive_path a full gdrive path <chr>, preferably with / at the end
#' @param overwrite overwrite files <lgl>? (default = FALSE)
#' @return no output, this function exerts a side-effect
#' @examples
#' upload_to_gdrive("/temporary_files/", "temp_files/", overwrite = TRUE)
# TODO: TEST
# TODO: ? use fs instead of base file handling

upload_to_gdrive <- function(local_path = NULL, gdrive_path = NULL, overwrite = FALSE){
    stopifnot(length(local_path) > 0,
              length(gdrive_path) > 0,
              dir.exists(local_path),
              is.logical(overwrite))

    # Run listing safely, so if fails, does not stop the function
    safe_drive_ls <- purrr::safely(googledrive::drive_ls)
    drive_list <- safe_drive_ls(gdrive_path)

    # Stop if the path contains files and can't overwrite
    stopifnot((nrow(drive_list$result) > 0) & overwrite == FALSE)

    # If the gdrive folder exists, and contains files, and can overwrite, delete all content
    if ((nrow(drive_list$result) > 0) & overwrite == TRUE) googledrive::drive_trash(gdrive_path)

    # If folder does not exist, create it
    if (!is.null(drive_list$error)) googledrive::drive_mkdir(gdrive_path)

    # Now that the folder is clean, upload the files
    # Beware that if overwrite = FALSE, and the folder is not empty, new files may have the same name as the old ones
        purrr::walk(
            list.files(local_path, pattern = ".csv", full.names = TRUE),
            ~ googledrive::drive_upload(
                media = .x,
                path = gdrive_path,
                type = "spreadsheet"
            )
        )
}
