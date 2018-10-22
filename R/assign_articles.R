#' Assign (or re-assign) articles to several team members
#'
#' Randomly assign articles to any number of team members for screening. The function will create a nesting variable (reviewer) that can be used to nest and save data.
#' @name assign_articles
#' @usage assign_articles(df, team_df = NULL, effort_col = NULL,
#' reviewers = 2, reassign = FALSE, seed = 1)
#' @param df data frame of articles, should have at least title and abstract columns. If the function is used for re-assigning, then there should be a decision, name, and position column as well.
#' @param team_df a data.frame of team members with name<chr>, and one or more effort <dbl> columns
#' @param effort_col variable name <chr> in the team file that specifies the effort of members
#' @param reviewers number of team members <num> for each article
#' @param reassign is it a reassigning of studies <lgl>?
#' @param seed a random seed <num> for reproducibility
#' @return A data frame that contains article data assigned to multiple team members for manual data entry.
#' @details
#' If the manual coding has already started, and some of the articles have been already processed, reassign = TRUE will preserve the work that has been done, and only assign unprocessed articles.
#' @importFrom rlang !! has_name sym
#' @importFrom dplyr pull rowwise mutate filter select one_of arrange bind_rows
#' @importFrom dplyr anti_join
#' @importFrom tidyr separate_rows drop_na
#'
#' @export
#'
#' @examples
#' library(dplyr)
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
# TODO: Refactor to remove redundancy (assigning is there twice)
# TODO: tidyevalize the effort parameter

assign_articles <- function(df,
                            team_df = NULL,
                            effort_col = NULL,
                            reviewers = 2,
                            reassign = FALSE,
                            seed = 1) {

    stopifnot(has_name(df, c("title", "abstract", "id")),
              has_name(team_df, c("name", effort_col)),
              is.numeric(seed),
              reviewers > 0,
              nrow(team_df) >= reviewers,
              is.numeric(pull(team_df, !!sym(effort_col))),
              sum(pull(team_df, !!sym(effort_col))) == 1,
              reassign == FALSE | has_name(df, c("decision", "name", "position"))
    )

    # Make distribution reproducible
    set.seed(seed)

    if (reassign == FALSE) {
        output <-
            df %>%
                rowwise() %>%
                # Assign two different reviewers to the article
                mutate(reviewer = sample(team_df$name,
                                         size = reviewers,
                                         prob = pull(team_df,
                                                     !!sym(effort_col))) %>%
                                         paste(collapse = ","),
                               position = paste("reviewer",
                                                1:reviewers,
                                                collapse = ",",
                                                sep = "")) %>%
                separate_rows(reviewer, position) %>%
                # Duplicate the reviewer variable, to keep name in df even after nesting
                mutate(name = reviewer)
    }

    if (reassign == TRUE) {
        # Get screened articles
        screened <-
            df %>%
            drop_na(decision) %>%
            mutate(reviewer = name)

        # Reassign the undecided articles
        reassigned <-
            df %>%
            # Get the undecided articles
            filter(is.na(decision)) %>%
            select(-one_of("name", "position")) %>%
            # Keep only one record for the article
            filter(!duplicated(id)) %>%
            # Reassign the articles
            rowwise() %>%
            # Assign several reviewers to the article
            mutate(reviewer = sample(team_df$name,
                                            size = reviewers,
                                            prob = pull(team_df,
                                                       !!sym(effort_col))) %>%
                                            paste(collapse = ","),
                          position = paste("reviewer",
                                           1:reviewers,
                                           collapse = ",",
                                           sep = "")) %>%
            separate_rows(reviewer, position) %>%
            # Duplicate the reviewer variable, to keep name in df even after nesting
            mutate(name = reviewer)

        # Remove the unnecessary reviewers that were reassigned to an article with decision
        reassigned_2rev <-
            reassigned %>%
            filter(id %in% screened$id) %>%
            anti_join(screened, by = c("id", "position")) %>%
            bind_rows(filter(reassigned,
                                       !(id %in% screened$id)),
                             .)

        # Return the articles, with screened articles first
        output <-
            bind_rows(screened, reassigned_2rev) %>%
            arrange(reviewer, decision, title)
    }

    output

}
