#' Adds new articles to ongoing screening process
#'
#' Add new literature to the existing and work-in-progress files and return an updated df. It preserves all previous screening effort, so no manual coding is lost.
#' @name add_to_screening
#' @usage add_to_screening(old_article_df, new_article_df, team_df, seed = 1)
#' @param old_article_df df that contains all work-in-progress data, retrieved by get_screening_sheets()
#' @param new_article_df a df that contains new articles, and has only one identifier
#' @param team_df data frame describing the team that contains name<chr>, and screening_effort<int> variables to be used when assigning the new articles
#' @param seed random seed to use to make the assignment reproducible
#' @return A data frame containing nested data frames for each revier with updated data
#' @examples
#' add_to_screening(old_articles, psycinfo_df, team, 1)
# TODO: Add error handling
# Possible bug!
# The old df contains the records in duplicate for a reason, so not all duplicates should be removed

library(tidyr)
library(dplyr)
source("R/assign_screening.R")

add_to_screening <- function(old_article_df, new_article_df, team_df, seed = 1){
    # The work-in-progress files are in duplicate, so duplicates should be removed first
    old_single <-
        old_article_df %>%
        filter(!(duplicated(id, incomparables = NA) |
                     duplicated(title, incomparables = NA)))
    # Then remove the overlap between the old and new articles
    bind_rows(old_single, new_article_df) %>%
        filter(!(duplicated(id, incomparables = NA) |
                     duplicated(title, incomparables = NA))) %>%
        # Keep only the new articles after removing all duplicates
        anti_join(old_single, by = c("title","id")) %>%
        # Remove position and name that came with the old df
        select(-one_of("position", "name")) %>%
        # Assign new reviewers based on the team_df
        assign_articles(team_df, seed) %>%
        # Add the old literature on the top, but first, re-add the reviewer column for nesting
        bind_rows(old_article_df %>% mutate(reviewer = name), .)
}
