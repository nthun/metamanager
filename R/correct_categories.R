#' Match strings with a pre-defined set of strings
#'
#' Correct strings to pre-defined strings. This is a wrapper for stringdist approimate string matching, where certain parameters are preset, and can be used easily in a tidyverse pipe. Using cosine matching to disregard word order.
#' @name correct_categories
#' @usage correct_categories(to_be_corrected, correct_terms)
#' @param to_be_corrected vector containing the strings to be corrected
#' @param correct_terms string vector containing the correct terms
#' @return A corrected string vector that can only contain the correct terms
#' @examples
#' articles %>% mutate(correct_reason = correct_categories(reason, read_lines("vg-meta-reasons.txt")))

library(stringdist)
library(dplyr)
library(stringr)

correct_categories <- function(to_be_corrected, correct_terms) {
    stopifnot(is.character(to_be_corrected),
              is.character(correct_terms))

    correct_terms[amatch(
                    to_be_corrected %>%
                        str_to_lower() %>%
                        str_replace("\\?", NA_character_),
                    correct_terms,
                    maxDist = 2,
                    method = "cosine"
    )]
}
