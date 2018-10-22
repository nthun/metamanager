#' Match strings with a pre-defined set of strings
#'
#' Correct strings to pre-defined strings. This is a wrapper for stringdist approimate string matching, where certain parameters are preset, and can be used easily in a tidyverse pipe. Using cosine matching to disregard word order.
#' @name correct_categories
#' @usage correct_categories(to_be_corrected = NULL, correct_terms = NULL,
#' max_dist = 2, method =  c("cosine", "osa", "lv", "dl", "hamming",
#' "lcs", "qgram", "jaccard", "jw", "soundex"), ...)
#' @param to_be_corrected vector containing the strings to be corrected
#' @param correct_terms string vector containing the correct terms
#' @param max_dist parameter passed down to stringdist::amatch() with a default
#' @param method parameter passed down to stringdist::amatch() with a default
#' @param ... further parameters to be passed down to stringdist::amatch()
#' @return A corrected string vector that can only contain the correct terms
#' @importFrom stringdist amatch
#' @importFrom stringr str_to_lower
#' @export
#'
#' @examples
#' library(dplyr)
# Create category names that define reasons for exclusion
#' reasons <- c("sample characteristics",
#'              "publication type",
#'              "manipulation",
#'              "other")
#'
#' # Create category names with typos
#' reasons_with_typo <- c("simple characteristisc",
#'                        "publication t",
#'                        "manuplation",
#'                        "o",
#'                        "publicaton type")
#'
#' # Create a dataset with random correct and incorrect categories in the "reason" column
#' df_with_typos <-
#'                  workaholism_pubmed %>%
#'                  mutate(decision = sample(c(0,1), size = nrow(.), replace = TRUE),
#'                         reason = if_else(decision == 0,
#'                                          NA_character_,
#'                          # Mix correct and incorrect categories
#'                                          sample(c(reasons, reasons_with_typo),
#'                                                 size = nrow(.),
#'                                                 replace = TRUE)
#'                                  )
#'                  )
#'
#' # The typos are corrected in a new column
#' mutate(df_with_typos, corrected_reason = correct_categories(reason, reasons))

correct_categories <-
    function(to_be_corrected = NULL,
             correct_terms = NULL,
             max_dist = 2,
             method = c("cosine", "osa", "lv", "dl", "hamming",
                        "lcs", "qgram", "jaccard", "jw", "soundex"),
             ...) {

        method <-  method[1]
        stopifnot(is.character(to_be_corrected),
                  is.character(correct_terms),
                  is.numeric(max_dist),
                  method %in% c("cosine", "osa", "lv", "dl", "hamming",
                                "lcs", "qgram", "jaccard", "jw", "soundex"))

        correct_terms[amatch(
            str_to_lower(to_be_corrected),
            str_to_lower(correct_terms),
            maxDist = max_dist,
            method = method,
            ...
        )]
    }
