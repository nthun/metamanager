#' Calculate inter-rater agreement statistics beween team members
#'
#' Calculate agreement summaries and Krippendorff's alpha for all reviewer pairs. Krippendorff's alpha can be calculated for nominal, ordinal, interval, and ratio scale variables.
#' @name calculate_agreement
#' @usage calculate_agreement(articles, kr_method = "nominal")
#' @param articles a data frame that contains the screened articles, and having a name<chr> column
#' @param kr_method The scale of the decision variable. Can be nominal (default), ordinal, interval, or ratio
#' @return A data frame containing agreement summaries, and the Krippendorff's alpha statistic for all pairs of screeners. invalid_decision<int> is the total number of decisions that are not 1 or 0.
#' @examples
#' calculate_agreement(articles)
# TODO: use explicit namespace

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(irr)
source("script/functions/tidy_kripp.R")

calculate_agreement <- function(articles, kr_method = "nominal"){
    stopifnot(has_name(articles, c("name","decision")))

    # Create a nested dataframe from all unrepeated combinations of names
    name_pairs <-
        articles %>%
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
                decision_table = map(
                    name_df,
                    ~ articles %>%
                        drop_na(decision) %>%
                        select(-position,-reason) %>%
                        filter(name %in% (.x %>% c())) %>%
                        spread(name, decision, sep = "_") %>%
                        set_names(str_replace(names(.), "name", "decision")) %>%
                        drop_na(contains("decision"))
                )) %>%
            # Drop rows that have no common articles
            filter(map(decision_table, ~nrow(.x)) > 0) %>%
            # Calculate the Krippendorff's alpha for all pairs
            transmute(
                name_pair,
                # Create tidy Krippendorff's alpha output tables for all pairs
                irr = map(
                    decision_table,
                    ~select(.x, starts_with("decision")) %>%
                        mutate_all(as.numeric) %>%
                        drop_na() %>%
                        t() %>%
                        irr::kripp.alpha(method = kr_method) %>%
                        tidy_kripp() %>%
                        as_data_frame()),
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
                                       magrittr::not() %>%
                                       sum())
            ) %>%
        # Return a tidy data frame
        unnest(irr) %>%
            right_join(name_pairs %>% select(name_pair), by = "name_pair")
    )
}
