#' Create a folder structure and a team file on goodle drive
#'
#' Create a folder structure on google drive for a meta-analysis project and a template team file in it that opens in the browser
#' @name init_meta
#' @usage init_meta(gdrive_path = NULL)
#' @param gdrive_path a full (new) gdrive path <chr>, preferably with / at the end
#' @return no output, this function exerts a side-effects
#' @examples
#' # Creating default folder structure and team file
#' init_meta("research/meta")
#' # TODO: A bug makes the function to initialize the gdrive_path directory twice, therefor it breaks halfway

init_meta <- function(gdrive_path = NULL){
    init_gdrive(gdrive_path)
    init_team(gdrive_path)
}
