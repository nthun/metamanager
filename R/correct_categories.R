#' Match strings with a pre-defined set of strings
#'
#' Correct strings to pre-defined strings. This is a wrapper for stringdist approimate string matching, where certain parameters are preset, and can be used easily in a tidyverse pipe. Using cosine matching to disregard word order.
#' @name correct_categories
#' @usage correct_categories(to_be_corrected, correct_terms)
#' @param to_be_corrected vector containing the strings to be corrected
#' @param correct_terms string vector containing the correct terms
#' @param max_dist parameter passed down to stringdist::amatch() with a default
#' @param method parameter passed down to stringdist::amatch() with a default
#' @param ... further parameters to be passed down to stringdist::amatch()
#' @return A corrected string vector that can only contain the correct terms
#' @examples
#' articles %>%
#'  mutate(correct_reason = correct_categories(reason, read_lines("vg-meta-reasons.txt")))

correct_categories <- function(to_be_corrected, correct_terms, max_dist = 2, method = "cosine", ...) {
    stopifnot(is.character(to_be_corrected),
              is.character(correct_terms))

    correct_terms[stringdist::amatch(
                        stringr::str_to_lower(to_be_corrected),
                        stringr::str_to_lower(correct_terms),
                        maxDist = max_dist,
                        method = method,
                        ...
                     )
                  ]
}
