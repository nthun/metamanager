#' Assign (or re-assign) articles to several team members
#'
#' Randomly assign articles to any number of team members for screening. The function will create a nesting variable (reviewer) that can be used to nest and save data.
#' @name assign_articles
#' @usage assign_articles(df = NULL, team_df = NULL, effort_col = NULL, reviewers = 2, reassign = FALSE, seed = 1)
#' @param df data frame of articles, should have at least title and abstract columns. If the function is used for re-assigning, then there should be a decision, name, and position column as well.
#' @param team_df a data.frame of team members with name<chr>, and one or more effort <dbl> columns
#' @param effort_col variable name <chr> in the team file that specifies the effort of members
#' @param reviewers number of team members <num> for each article
#' @param reassign is it a reassigning of studies <lgl>?
#' @param seed a random seed <num> for reproducibility
#' @return A data frame that contains article data assigned to multiple team members for manual data entry.
#' @details
#' If the manual coding has already started, and some of the articles have been already processed, reassign = TRUE will preserve the work that has been done, and only assign unprocessed articles.
#' @examples
#' library(magrittr)
#' library(tibble)
#'
#' team_df <- tibble(name = c("Tom", "Jerry", "Rose"),
#'                   screening_effort = c(.6, .3, .1))
#'
#' workaholism_pubmed %>%
#'   # First, keep only one identifier for each article
#'   make_id(identifier = c("doi", "pmid")) %>%
#'   assign_articles(team_df = team_df,
#'                   reviewers = 2,
#'                   effort_col = "screening_effort")
#' TODO: Refactor to remove redundancy (assigning is there twice)
#' TODO: tidyevalize the effort parameter

assign_articles <- function(df,
                            team_df = NULL,
                            effort_col,
                            reviewers = 2,
                            reassign = FALSE,
                            seed = 1) {

    stopifnot(tibble::has_name(df,
                               c("title", "abstract", "id")),
              tibble::has_name(team_df,
                               c("name", effort_col)),
              is.numeric(seed),
              reviewers > 0,
              nrow(team_df) >= reviewers,
              is.numeric(dplyr::pull(team_df, !!sym(effort_col))),
              sum(dplyr::pull(team_df, !!sym(effort_col))) == 1,
              reassign == FALSE | tibble::has_name(df,
                                                   c("decision", "name", "position"))
    )

    # Make distribution reproducible
    set.seed(seed)

    if (reassign == FALSE) {
        output <-
            df %>%
                dplyr::rowwise() %>%
                # Assign two different reviewers to the article
                dplyr::mutate(reviewer = sample(team_df$name,
                                                size = reviewers,
                                                prob = dplyr::pull(team_df,
                                                                   !!sym(effort_col))) %>%
                                         paste(collapse = ","),
                               position = paste("reviewer",
                                                1:reviewers,
                                                collapse = ",",
                                                sep = "")) %>%
                tidyr::separate_rows(reviewer,
                                     position) %>%
                # Duplicate the reviewer variable, to keep name in df even after nesting
                dplyr::mutate(name = reviewer)
    }

    if (reassign == TRUE) {
        # Get screened articles
        screened <-
            df %>%
            tidyr::drop_na(decision) %>%
            dplyr::mutate(reviewer = name)

        # Reassign the undecided articles
        reassigned <-
            df %>%
            # Get the undecided articles
            dplyr::filter(is.na(decision)) %>%
            dplyr::select(-tidyselect::one_of("name", "position")) %>%
            # Keep only one record for the article
            dplyr::filter(!duplicated(id)) %>%
            # Reassign the articles
            dplyr::rowwise() %>%
            # Assign several reviewers to the article
            dplyr::mutate(reviewer = sample(team_df$name,
                                            size = reviewers,
                                            prob = dplyr::pull(team_df,
                                                               !!rlang::sym(effort_col))) %>%
                                            paste(collapse = ","),
                          position = paste("reviewer",
                                           1:reviewers,
                                           collapse = ",",
                                           sep = "")) %>%
            tidyr::separate_rows(reviewer,
                                 position) %>%
            # Duplicate the reviewer variable, to keep name in df even after nesting
            dplyr::mutate(name = reviewer)

        # Remove the unnecessary reviewers that were reassigned to an article with decision
        reassigned_2rev <-
            reassigned %>%
            dplyr::filter(id %in% screened$id) %>%
            dplyr::anti_join(screened,
                             by = c("id", "position")) %>%
            dplyr::bind_rows(dplyr::filter(reassigned,
                                           !(id %in% screened$id)),
                             .)

        # Return the articles, with screened articles first
        output <-
            dplyr::bind_rows(screened,
                             reassigned_2rev) %>%
            dplyr::arrange(reviewer,
                           decision,
                           title)
    }

    output

}
