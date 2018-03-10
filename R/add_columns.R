#' Add named empty columns to a dataframe
#'
#' Add columns to a dataframe that can be used for manual coding
#' @name add_columns
#' @usage add_columns(df, columns)
#' @param df data frame of articles
#' @param columns a vector of column names <str> to be added
#' @param before  <lgl> Should the new variables be in the beginning of new df (default)
#' @return A data frame that contains the article info with empty columns for manual data entry
#' @examples
#' add_columns(articles, c("decision","reason"), before = TRUE)
#'
#' newcol <- c("x","y")
#' iris %>%
#'     as_tibble() %>%
#'     add_columns(newcol, before = FALSE) %>%
#'     select()

add_columns <- function(df, columns, before = TRUE){
    stopifnot(  is.data.frame(df),
                length(columns) > 0)
    varnames <- rep(NA_character_, length(columns))
    names(varnames) <- columns
    df <- dplyr::mutate(df, !!! varnames)
    if (before == TRUE) dplyr::select(df, !! columns, everything()) else df
}



