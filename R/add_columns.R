#' Add named empty columns to a dataframe
#'
#' Add columns to a dataframe that can be used for manual coding
#' @name add_columns
#' @usage add_columns(df = NULL, columns = NULL, before = TRUE)
#' @param df data frame of articles
#' @param columns a vector of column names <chr> to be added
#' @param before  <lgl> Should the new variables be in the beginning of new df (default)
#' @return A data frame that contains the article info with empty columns for manual data entry
#' @importFrom dplyr mutate select intersect everything
#' @importFrom rlang !! !!!
#' @export
#'
#' @examples
#' add_columns(df = workaholism_scopus,
#'             columns = c("decision","reason"),
#'             before = TRUE)

add_columns <- function(df = NULL,
                        columns = NULL,
                        before = TRUE){
    stopifnot(  is.data.frame(df),
                length(intersect(names(df), columns)) == 0,
                length(columns) > 0)

    varnames <- rep(NA_character_, length(columns))
    names(varnames) <- columns
    new_df <- mutate(df, !!!varnames)

    if (before == TRUE) select(new_df, !!columns, everything()) else new_df
}
