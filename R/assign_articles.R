#' Assign articles for screening to raters reproducibly
#'
#' Randomly assign articles to two different team members for screening. The function will create files for each team member. The files will contain article data with the first two rows being the decision and reason columns, that can be filled manually during the screening.
#' @name assign_articles
#' @usage assign_articles(df, team_df, effort_col, reviewers = 2, seed = 1)
#' @param df data frame of articles, should have at least title<str> and abstract<str> columns
#' @param team_df a data.frame of team members with name<chr>, and one or more effort <dbl> columns
#' @param effort_col variable name <chr> in the team file that specifies the effort of members
#' @param reviewers Number of team members <num> for each article
#' @param seed a random seed <num> for reproducibility
#' @return A data frame that contains the article info with variables to use for manual screening, and the assigned reviewers
#' @examples
#' # Assign articles for screening to 2 reviewers
#' assign_articles(iris, team_df, "screening_effort", reviewer = 2, seed = 1)

assign_articles <- function(df, team_df, effort_col, reviewers = 2, seed = 1){
    stopifnot(tibble::has_name(df, c("title", "abstract")),
              tibble::has_name(team_df, c("name", effort_col)),
              is.numeric(seed),
              reviewers > 0,
              nrow(team_df) >= reviewers,
              sum(team_df[, effort_col]) == 1)

    # Make distribution reproducible
    set.seed(seed)

    df %>%
        dplyr::rowwise() %>%
        # Assign two different reviewers to the article
        dplyr::mutate(reviewer = sample(team_df$name, size = reviewers,
                                 prob = dplyr::pull(team_df, !!effort_col)) %>%
                          paste(., collapse = ","),
               position = paste("reviewer", 1:reviewers, collapse = ",", sep = "")) %>%
        tidyr::separate_rows(reviewer, position) %>%
        # Duplicate the reviewer variable, to keep name in df even after nesting
        dplyr::mutate(name = reviewer)
}
