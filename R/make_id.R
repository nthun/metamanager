#' Make a tidy unique identifier in a data frame of articles
#'
#' Select the best id based on a pre-defined hierarchy, and keep only that for the article
#' @name make_id
#' @usage make_id(df, identifier = c("doi","pmid","psyid"))
#' @param df data frame that has identifiers
#' @param identifier a vector of identifiers <chr> in decreasing order by importance
#' @return A data frame without individual id columns, and with new identifier and id columns that contain the best available identifier and the id, respectively
#' @examples
#' make_id(df, c("doi","pmid","psyid"))

library(tidyr)
library(dplyr)

make_id <- function(df,
                    identifier = c("doi","pmid","psyid")){
    # Stop if there are no valid identifiers in the data frame
    stopifnot(is.data.frame(df),
              length(intersect(names(df), identifier)) > 0)
    # Create a tbl of identifiers for merging
    id_hierarchy <- tibble(identifier = identifier,
                           id_rank = seq(1L, length(identifier), 1L))

    df %>%
        # Gather all available identifiers
        gather(identifier, id, intersect(names(.), identifier)) %>%
        drop_na(id) %>%
        # Get information about the rank of the identifier
        left_join(id_hierarchy, by = "identifier") %>%
        group_by(title) %>%
        # Find the best available identifier for the article
        mutate(best_id = min(id_rank)) %>%
        ungroup() %>%
        # Remove all identifiers that are less important
        filter(id_rank == best_id) %>%
        # Remove clutter and rearrange variables
        select(-id_rank, -best_id) %>%
        select(identifier, id, source, everything())
}





