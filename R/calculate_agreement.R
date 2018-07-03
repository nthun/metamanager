#' Calculate inter-rater agreement statistics beween team members
#'
#' Calculate agreement summaries and Krippendorff's alpha for all reviewer pairs. Krippendorff's alpha can be calculated for nominal, ordinal, interval, and ratio scale variables.
#' @name calculate_agreement
#' @usage calculate_agreement(articles, decision_col = "decision", kr_method = "nominal")
#' @param articles a data frame that contains the screened articles, and having a name<chr> column
#' @param decision_col variable name <chr> that defines the decision column (0/1)
#' @param kr_method The scale of the decision variable. Can be nominal (default), ordinal, interval, or ratio
#' @return A data frame containing agreement summaries, and the Krippendorff's alpha statistic for all pairs of screeners. invalid_decision<int> is the total number of decisions that are not 1 or 0.
#' @examples
#' calculate_agreement(articles)

library(dplyr)

calculate_agreement <- function(articles, decision_col = "decision", kr_method = "nominal"){
    stopifnot(tibble::has_name(articles, c("name", decision_col)))

    # Create a nested dataframe from all unrepeated combinations of names
    name_pairs <-
        articles %>%
        dplyr::distinct(name) %>%
        dplyr::pull() %>%
        utils::combn(., 2) %>%
        t() %>%
        tibble::as_data_frame() %>%
        purrr::set_names(c("name1", "name2")) %>%
        dplyr::mutate(name_pair = paste(name1, name2, sep = "_")) %>%
        dplyr::group_by(name_pair) %>%
        tidyr::nest(.key = name_df)

    # Suppress coertion warnings which are the result of invalid decisions, info is not lost as invalid decision is returned in separate variable
    suppressWarnings(
        name_pairs %>%
            # Create tables for decision and reason separately
            dplyr::mutate(
                decision_table = purrr::map(
                    name_df,
                    ~ articles %>%
                        tidyr::drop_na(!!decision_col) %>%
                        dplyr::select(-position,-reason) %>%
                        dplyr::filter(name %in% (.x %>% c())) %>%
                        tidyr::spread(name, !!decision, sep = "_") %>%
                        purrr::set_names(stringr::str_replace(names(.), "name", "decision")) %>%
                        tidyr::drop_na(contains("decision"))
                )) %>%
            # Drop rows that have no common articles
            dplyr::filter(purrr::map(decision_table, ~nrow(.x)) > 0) %>%
            # Calculate the Krippendorff's alpha for all pairs
            dplyr::transmute(
                name_pair,
                # Create tidy Krippendorff's alpha output tables for all pairs
                kr_alpha = purrr::map(
                    decision_table,
                    ~select(.x, starts_with("decision")) %>%
                        dplyr::mutate_all(as.numeric) %>%
                        tidyr::drop_na() %>%
                        t() %>%
                        irr::kripp.alpha(method = kr_method) %>%
                        tidy_kripp() %>% # From {metamanager}
                        tibble::as_data_frame()),
                # Create summary statistics for all pairs
                include_both = purrr::map_int(decision_table,
                                       ~dplyr::select(.x, dplyr::starts_with("decision")) %>%
                                           dplyr::filter(.[[1]] == 1 & .[[2]] == 1) %>%
                                           nrow()),
                exclude_both = purrr::map_int(decision_table,
                                       ~dplyr::select(.x, dplyr::starts_with("decision")) %>%
                                           dplyr::filter(.[[1]] == 0 & .[[2]] == 0) %>%
                                           nrow()),
                no_agreement = purrr::map_int(decision_table,
                                       ~dplyr::select(.x, dplyr::starts_with("decision")) %>%
                                           dplyr::filter(.[[1]] != .[[2]]) %>%
                                           nrow()),
                # The number of all invalid decisions for both screeners of the pair summarised
                invalid_decision = purrr::map_int(decision_table,
                                   ~dplyr::select(.x, dplyr::starts_with("decision")) %>%
                                       c(recursive = TRUE) %>%
                                       stringr::str_detect("1|0") %>%
                                       `!` %>% # Not
                                       sum())
            ) %>%
        # Return a tidy data frame
        unnest(irr) %>%
            dplyr::right_join(name_pairs %>% dplyr::select(name_pair), by = "name_pair")
    )
}
