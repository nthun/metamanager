#' Create folder structure on goodle drive
#'
#' Create a folder structure (default or custom) on google drive for a meta-analysis project
#' @name init_gdrive
#' @usage init_gdrive(gdrive_path = NULL,
#'                    folders = c("literature_search/",
#'                                "screening/",
#'                                "screening_consensus/",
#'                                "fulltext/",
#'                                "extraction/",
#'                                "extraction_consensus/"))
#' @param gdrive_path a full (new) gdrive path <chr>, preferably with / at the end
#' @param folders a vector <chr> of folder names to create in path
#' @return no output, this function exerts a side-effect
#' @importFrom purrr safely walk
#' @importFrom googledrive drive_ls drive_mkdir
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Creating default folder structure
#' init_gdrive("research/meta-analysis/")
#' # Creating custom folder structure
#' init_gdrive("research/meta-analysis/", c("screening", "extract"))
#' }

init_gdrive <-
    function(gdrive_path = NULL,
             folders = c(
                 "literature_search/",
                 "screening/",
                 "screening_consensus/",
                 "fulltext/",
                 "extraction/",
                 "extraction_consensus/"
             )) {

    stopifnot(length(gdrive_path) > 0)

    # Run listing safely, so if fails, does not stop the function
    safe_drive_ls <- safely(drive_ls)
    drive_list <- safe_drive_ls(gdrive_path)

    # Stop if there is en error
    stopifnot((nrow(drive_list$result) > 0))

    all_path <- paste(gdrive_path, folders, sep = "/")
    all_path <- create_path_structure(all_path)
    walk(all_path, ~drive_mkdir(name = .x))
}








