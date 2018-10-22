#' Calculate inter-rater agreement statistics beween team members
#'
#' Calculate agreement summaries and Krippendorff's alpha for all reviewer pairs. Krippendorff's alpha can be calculated for nominal, ordinal, interval, and ratio scale variables.
#' @name calculate_agreement
#' @usage calculate_agreement(df, decision_col = "decision", kr_method = "nominal")
#' @param df a data frame that contains the screened articles, and having a name<chr> column
#' @param decision_col variable name <chr> that defines the decision column (0/1)
#' @param kr_method The scale of the decision variable. Can be nominal (default), ordinal, interval, or ratio
#' @return A data frame containing agreement summaries, and the Krippendorff's alpha statistic for all pairs of screeners. invalid_decision<int> is the total number of decisions that are not 1 or 0.
#' @importFrom rlang !! has_name set_names
#' @importFrom dplyr distinct pull as_data_frame mutate group_by select filter
#' @importFrom dplyr transmute mutate_all starts_with right_join rename
#' @importFrom utils combn
#' @importFrom tidyr nest drop_na spread unnest
#' @importFrom purrr map map_int
#' @importFrom stringr str_replace str_detect
#' @importFrom irr kripp.alpha
#' @seealso kripp.alpha
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' team_df <- tibble(name = c("Tom", "Jerry", "Rose"),
#'                   screening_effort = c(.5, .4, .1))
#'
#' set.seed(1)
#' df <-
#'     workaholism_pubmed %>%
#'     make_id(identifier = c("doi", "pmid")) %>%
#'     assign_articles(team_df = team_df,
#'                     reviewers = 2,
#'                     effort_col = "screening_effort") %>%
#'     # Generate fake screening data
#'     mutate(decision = sample(c(0:1, NA_integer_),
#'                              prob = c(.6,.3, .1),
#'                              size = nrow(.),
#'                              replace = TRUE))
#'
#' calculate_agreement(df, decision_col = "decision")

calculate_agreement <- function(df,
                                decision_col = "decision",
                                kr_method = "nominal"){

    stopifnot(has_name(df, c("name", decision_col)))

    # Create a nested dataframe from all unrepeated combinations of names
    name_pairs <-
        df %>%
        distinct(name) %>%
        pull() %>%
        combn(., 2) %>%
        t() %>%
        as_data_frame() %>%
        set_names(c("name1", "name2")) %>%
        mutate(name_pair = paste(name1, name2, sep = "_")) %>%
        group_by(name_pair) %>%
        nest(.key = name_df)

    # Suppress coertion warnings which are the result of invalid decisions, info is not lost as invalid decision is returned in separate variable
    suppressWarnings(
        name_pairs %>%
            # Create tables for decision and reason separately
            mutate(
                decision_table = map(name_df,
                                     ~ df %>%
                                         drop_na(!!decision_col) %>%
                                         select(-reviewer, -position) %>%
                                         filter(name %in% c(.x$name1, .x$name2)) %>%
                                         spread(name, !!decision_col, sep = "_") %>%
                                         set_names(str_replace(names(.), "name", "decision")) %>%
                                         drop_na(contains("decision"))
                )) %>%
            # Drop rows that have no common articles
            filter(map_int(decision_table,
                       ~nrow(.x)) > 0) %>%
            # Calculate the Krippendorff's alpha for all pairs
            transmute(
                name_pair,
                # Create summary statistics for all pairs
                include_both = map_int(decision_table,
                                       ~select(.x, starts_with("decision")) %>%
                                       filter(.[[1]] == 1 & .[[2]] == 1) %>%
                                        nrow()),
                exclude_both = map_int(decision_table,
                                       ~select(.x, starts_with("decision")) %>%
                                        filter(.[[1]] == 0 & .[[2]] == 0) %>%
                                        nrow()),
                no_agreement = map_int(decision_table,
                                       ~select(.x, starts_with("decision")) %>%
                                        filter(.[[1]] != .[[2]]) %>%
                                        nrow()),
                # The number of all invalid decisions for both screeners of the pair summarised
                invalid_decision = map_int(decision_table,
                                   ~select(.x, starts_with("decision")) %>%
                                       c(recursive = TRUE) %>%
                                       str_detect("1|0") %>%
                                       `!` %>% # Not
                                       sum()),
                #Create tidy Krippendorff's alpha output tables for all pairs
                kr.test = map(decision_table,
                                   ~select(.x, starts_with("decision")) %>%
                                       mutate_all(as.numeric) %>%
                                       drop_na() %>%
                                       t() %>%
                                       kripp.alpha(method = kr_method) %>%
                                       tidy_kripp() %>% # From {metamanager}
                                       as_data_frame())
            ) %>%
        unnest(kr.test) %>%
        select(-method, -raters)

    )
}
