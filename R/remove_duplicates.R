#' @name remove_duplicates
#'
#' Removes duplicate records that have the same values in the identification information.
#' @usage remove_duplicates(df, keys)
#'
#' @param df data frame with potential duplicates
#' @param keys a character vector of identifier variables in the data frame that
#'
#' @return A data frame without duplicate elements
#' @examples remove_duplicates(df, c("doi","pmid"))

remove_duplicates <- function(df, keys){

    stopifnot(is.data.frame(df),
              is.character(keys),
              length(keys) > 0,
              all(rlang::has_name(df, keys)))

    # Remove whitespace and convert key variables to lowercase for the filtering of duplicates
    # These changes are not present in the final dataframe
    dplyr::filter_at(df, vars(!!keys), dplyr::all_vars(stringr::str_squish(.) %>%
                                                           stringr::str_to_lower() %>%
                                                           duplicated(incomparables = NA) %>%
                                                           magrittr::not()))
}
