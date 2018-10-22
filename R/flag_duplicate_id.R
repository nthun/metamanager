#' Flag duplicates by identifiers
#'
#' Flags articles in a dataframe that are duplicated and has the same identifier
#' @name flag_duplicate_id
#' @usage flag_duplicate_id(df, keys = NULL)
#'
#' @param df a data frame with potential duplicates
#' @param keys a character vector of identifier variables in the data frame
#'
#' @return The original data frame is returned with a new column "duplicate_by_id", that can be 0 or 1
#' @importFrom dplyr any_vars vars if_else mutate transmute filter_at left_join
#' @importFrom stringr str_squish str_to_lower
#' @importFrom rlang !! !!! has_name syms
#'
#' @export
#'
#' @examples
#' # Show all articles with duplicated ids
#' library(dplyr)
#' merge_sources(workaholism_psychinfo, workaholism_pubmed, workaholism_scopus,
#'               .renames = c("journal" = "publication")) %>%
#'  flag_duplicate_id(keys = c("psyid", "pmid", "doi", "eid", "sid")) %>%
#'  filter(duplicate_by_id == 1)

flag_duplicate_id <- function(df,
                              keys = NULL){

    stopifnot(is.data.frame(df),
              is.character(keys),
              length(keys) > 0,
              all(has_name(df, keys)))

    # Remove whitespace and convert key variables to lowercase for the filtering of duplicates
    # These changes are not present in the duplicate removed dataframe
    df %>%
    filter_at(.vars = vars(!!keys),
              .vars_predicate = any_vars(str_squish(.) %>%
                                         str_to_lower() %>%
                                         duplicated(incomparables = NA))) %>%
    # Keep ony the keys and duplicate info
    transmute(!!!syms(keys),
              duplicate_by_id = 1L) %>%
    # Join the duplicate info back to the original df, using all keys
    left_join(df, ., by = keys) %>%
    # Fill NA with 0-s
    mutate(duplicate_by_id = if_else(is.na(duplicate_by_id),
                                     0L,
                                     duplicate_by_id))
}



