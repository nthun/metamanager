#' Flag duplicates by identifiers
#'
#' Flags articles in a dataframe that are duplicated and has the same identifier
#' @name flag_duplicates_by_ids
#' @usage flag_duplicates_by_ids(df, keys = NULL)
#'
#' @param df a data frame with potential duplicates
#' @param keys a character vector of identifier variables in the data frame
#'
#' @return The original data frame is returned with a new column "duplicate_by_id", that can be 0 or 1
#' @seealso \code{\link{flag_duplicates_by_title}} for flagging by title
#' @examples
#' flag_duplicates_by_ids(df, c("doi","pmid","psyid","eid"))

flag_duplicates_by_ids <- function(df, keys = NULL){

    stopifnot(is.data.frame(df),
              is.character(keys),
              length(keys) > 0,
              all(rlang::has_name(df, keys)))

    # Remove whitespace and convert key variables to lowercase for the filtering of duplicates
    # These changes are not present in the duplicate removed dataframe
    dplyr::filter_at(df, vars(!!keys),
                     dplyr::any_vars(stringr::str_squish(.) %>%
                                         stringr::str_to_lower() %>%
                                         duplicated(incomparables = NA))) %>%
    # Keep ony the keys and duplicate info
    dplyr::transmute(!!!syms(keys),
                     duplicate_by_id = 1) %>%
    # Join the duplicate info back to the original df, using all keys
    dplyr::left_join(df, ., by = keys) %>%
    # Fill NA with 0-s
    dplyr::mutate(duplicate_by_id = dplyr::if_else(is.na(duplicate_by_id),
                                                  0L,
                                                  duplicate_by_id))
}



