#' Make a tidy unique identifier in a data frame of articles
#'
#' Select the best id based on a pre-defined hierarchy, and keep only that for the article
#' @name make_id
#' @usage make_id(df, identifier = c("doi","pmid","psyid"))
#' @param df data frame that has identifiers as separate variables
#' @param identifier a vector of identifiers <chr> in decreasing order by importance
#' @return A data frame without individual id columns, and with new identifier and id columns that contain the best available identifier and the id, respectively
#' @examples
#' make_id(df, c("doi","pmid","psyid"))

make_id <- function(df,
                    identifier = c("doi","pmid","psyid")){
    # Stop if there are no valid identifiers in the data frame
    stopifnot(is.data.frame(df),
              length(dplyr::intersect(names(df), identifier)) > 0)

    # Create a tbl of identifiers for merging
    id_hierarchy <- tibble::tibble(identifier = identifier,
                           id_rank = seq(1L, length(identifier), 1L))

    df %>%
        # Make a temporal id so articles will be groupable after gather
        dplyr::mutate(temp_id = dplyr::row_number()) %>%
        # Gather all available identifiers
        tidyr::gather(identifier,
                      id,
                      dplyr::intersect(names(.), identifier)) %>%
        tidyr::drop_na(id) %>%
        # Get information about the rank of the identifier
        dplyr::left_join(id_hierarchy,
                         by = "identifier") %>%
        # Keep only the lowest ranking identifier for each article
        dplyr::group_by(temp_id) %>%
        dplyr::arrange(id_rank) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        # Remove helper variables
        dplyr::select(-id_rank, -temp_id) %>%
        # Rearrange order of columns
        dplyr::select(identifier, id, dplyr::everything())
}
