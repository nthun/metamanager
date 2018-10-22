#' Merge data sources
#'
#' Merge all sources into a single data frame with tidy names. A named vector can be provided for renaming variables.
#' @name merge_sources
#' @usage merge_sources(..., .renames = NULL, .tidy_names = TRUE, .all_char = TRUE)
#'
#' @param ... names of data dataframes to be merged
#' @param .renames a named vector that contatins all renames for all datafiles
#' @param .tidy_names should the names become tidy?
#' @param .all_char convert all variables to character?
#'
#' @return a dataframe that contains all data sources from all files
#' @details For renames, c("new_name" = "old_name") format should be used.
#' @importFrom rlang dots_n have_name set_names dots_list
#' @importFrom purrr map
#' @importFrom dplyr rename_all mutate_all bind_rows
#' @importFrom stringr str_to_lower
#' @export
#'
#' @examples
#' merge_sources(workaholism_psychinfo, workaholism_pubmed, workaholism_scopus,
#'                         .renames = c(journal = "publication"))

merge_sources <- function(...,
                          .renames = NULL,
                          .tidy_names = TRUE,
                          .all_char = TRUE){

    # Error handling
    stopifnot(dots_n() == 0,
              (is.null(.renames) | all(have_name(.renames))),
              is.logical(.tidy_names),
              is.logical(.all_char)
              )

    # plyr has the opposite logic as dplyr, so new and old values should be swapped in the vector
    if (!is.null(.renames))
    ordered_renames <-
        names(.renames) %>%
        set_names(.renames)

    # Capture dots
    dts <- dots_list(...)

    # Make variable names tidy before merging
    if (isTRUE(.tidy_names))
        dts <- map(dts,
                   ~rename_all(.x, str_to_lower))

    # Convert all variables to character to ensure no information is lost
    if (isTRUE(.all_char))
        dts <- map(dts,
                   ~mutate_all(.x, as.character))

    # Rename variables, if there are any to rename
    if (!is.null(.renames))
        dts <- map(dts,
                  ~plyr::rename(.x, ordered_renames))
    # Put everything into one dataframe instead of separate lists
    bind_rows(dts)
}


