#' Assign articles for screening to raters reproducibly
#'
#' Randomly assign articles to two different team members for screening. The function will create files for each team member. The files will contain article data with the first two rows being the decision and reason columns, that can be filled manually during the screening.
#' @name assign_screening
#' @param df data frame of articles
#' @param team a data.frame of team members with name<chr>, and effort <dbl>:0-1 variables
#' @param seed a random seed <int> for reproducibility
#' @return A data frame that contains the article info and the assigned reviewers
#' @examples
#' assign_screening(merged_articles, team_df, 1)
# TODO: Feature: Possibility to assign an article to more than two reviewers
library(tidyr)
library(dplyr)
library(glue)
library(tibble)

assign_screening <- function(articles, team_df, seed = 1){
    stopifnot(has_name(articles, c("title", "abstract")),
              has_name(new_team_df, c("name","screening_effort")),
              is.numeric(seed),
              sum(team_df$screening_effort) == 1)

    # Make distribution reproducible
    set.seed(seed)
    articles %>%
        rowwise() %>%
        # Assign two different reviewers to the article
        mutate(reviewer1 = sample(team_df$name, size = 1, prob = team_df$screening_effort)) %>%
        mutate(reviewer2 = sample(team_df$name[team_df$name != reviewer1], size = 1, prob = team_df$screening_effort[team_df$name != reviewer1])) %>%
        gather(position, reviewer, reviewer1:reviewer2) %>%
        # Add columns for the manual screening
        mutate(decision = NA_character_,
               reason = NA_character_) %>%
        select(decision, reason, title, abstract, everything()) %>%
        group_by(reviewer) %>%
        # Duplicate the reviewer variable, to keep name in df even after nesting
        mutate(name = reviewer) %>%
        ungroup()
}
