#' Add named empty columns to a dataframe
#'
#' Add columns to a dataframe that can be used for manual coding
#' @name add_columns
#' @usage add_columns(df, columns)
#' @param df data frame of articles
#' @param columns a vector of column names <str> to be added
#' @return A data frame that contains the article info with empty columns for manual data entry
#' @examples
#' add_columns(articles, c("decision","reason"))

library(dplyr)

add_columns <- function(df, columns){
    varnames <- rep(NA_character_, length(columns))
    names(varnames) <- columns
    mutate(df, !!! varnames)
}

