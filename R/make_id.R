#' Make a tidy unique identifier in a data frame of articles
#'
#' Select the best id based on a pre-defined hierarchy, and keep only that for the article
#' @name make_id
#' @usage make_id(df, .id_hierarchy = id_hierarchy)
#' @param df data frame that has identifiers
#' @param .id_hierarchy data frame with identifier<chr>, and rank<num> columns that define the rank (importance) of an identifier. Smaller number means more important
#' @return A data frame without individual id columns, and with new identifier and id columns that contain the best available identifier and the id, respectively
#' @examples
#' make_id(df)

library(tidyr)
library(dplyr)
id_hierarchy <- tibble(identifier = c("doi","pmid","psyid","eid","pq_id","no_id"),
                       id_rank = c(1, 2, 3, 4, 5, 6))

make_id <- function(df, .id_hierarchy = id_hierarchy){
    # Stop if there are no valid identifiers in the data frame
    stopifnot(names(df) %>%
                  intersect(id_hierarchy$identifier) %>%
                  length() > 0)

    df %>%
        # Gather all available identifiers
        gather(identifier, id, intersect(names(.), id_hierarchy$identifier)) %>%
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
