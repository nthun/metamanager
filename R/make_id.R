#' Make a tidy unique identifier in a data frame of articles
#'
#' Select the best id based on a pre-defined hierarchy, and keep only that for the article
#' @name make_id
#' @usage make_id(df, identifier = c("doi","pmid","psyid"))
#' @param df data frame that has identifiers as separate variables
#' @param identifier a vector of identifiers <chr> in decreasing order by importance
#' @return A data frame without individual id columns, and with new identifier and id columns that contain the best available identifier and the id, respectively
#' @importFrom dplyr tibble intersect mutate left_join group_by arrange slice
#' @importFrom dplyr ungroup row_number everything select
#' @importFrom tidyr gather drop_na
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' merge_sources(workaholism_pubmed, workaholism_psychinfo) %>%
#'   make_id(c("psyid", "pmid", "doi", "eid", "sid"))

make_id <- function(df,
                    identifier = c("doi","pmid","psyid")){
    # Stop if there are no valid identifiers in the data frame
    stopifnot(is.data.frame(df),
              length(intersect(names(df), identifier)) > 0)

    # Create a tbl of identifiers for merging
    id_hierarchy <- tibble(identifier = identifier,
                           id_rank = seq(1L, length(identifier), 1L))

    df %>%
        # Make a temporal id so articles will be groupable after gather
        mutate(temp_id = row_number()) %>%
        # Gather all available identifiers
        gather(identifier,
               id,
               intersect(names(.), identifier)) %>%
        drop_na(id) %>%
        # Get information about the rank of the identifier
        left_join(id_hierarchy, by = "identifier") %>%
        # Keep only the lowest ranking identifier for each article
        group_by(temp_id) %>%
        arrange(id_rank) %>%
        slice(1) %>%
        ungroup() %>%
        # Remove helper variables
        select(-id_rank, -temp_id) %>%
        # Rearrange order of columns
        select(identifier, id, everything())
}
