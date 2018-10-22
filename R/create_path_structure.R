#' Create a full path structure
#'
#' Create a full hierarchical path structure based on a vector of folder names with incomplete hierarchical definition
#' @name create_path_structure
#' @usage create_path_structure(folders = NULL)
#' @param folders a vector of paths <chr> that does not have to contain all folder levels separately
#' @return a vctor <chr> of paths that fully define a folder structure
#' @source Cumulative paste function is based on https://stackoverflow.com/questions/24862046/cumulatively-paste-concatenate-values-grouped-by-another-variable created by https://stackoverflow.com/users/2414948/alexis-laz
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate row_number filter group_by ungroup distinct pull
#' @importFrom tidyr gather
#'
#' @export
#'
#' @examples
#' # Creating default folder structure
#' create_path_structure(c("research/meta/extraction","research/meta/screening"))

create_path_structure <- function(folders = NULL){
    stopifnot(length(folders) > 0)

    cumulative_paste <- function(x, .sep = " ")
        Reduce(function(x1, x2) paste(x1, x2, sep = .sep), x, accumulate = TRUE)

    folders %>%
        str_split("/", simplify = TRUE) %>%
        as_tibble() %>%
        # Add an identifier to path so later grouping can be based on separathe paths
        mutate(path_id = row_number()) %>%
        # Gather into long format
        gather(level, folder, -path_id) %>%
        # Drop empty folder names
        filter(folder != "") %>%
        # Cumulatively paste folder names
        group_by(path_id) %>%
        mutate(full_path = cumulative_paste(folder, .sep = "/")) %>%
        ungroup() %>%
        # Remove redundant folders
        distinct(full_path, .keep_all = TRUE) %>%
        pull(full_path)
}
