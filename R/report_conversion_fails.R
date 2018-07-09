# TODO: make this one single function for all conversion checks that contains all required conversions in one step. May require a list format for columns and funs!
#' Checks for coversion errors in nested dataframes
#'
#' Checks if a specific conversion produces any errors in the nested dfs that has (partly) the same variable names.
#' @name report_conversion_fails
#' @usage report_conversion_fails(df, columns, convert_to)
#'
#' @param df data frame of nested data frames
#' @param columns a list of vectors that contain column names <chr> to be converted
#' @param convert_to a list of outcput variable types for each vector of columns. Can be "integer", "logical", "double" (character and factor conversion does not produce errors)
#'
#' @return A tibble that shows all conversion fails for all type conversions for each file
#' @examples
#'
#' df <-
#'   tibble::tibble(file = c("file1","file1", "file2", "file2", "file2"),
#'                  x = c(1,2,"c", 4, "d"),
#'                  y = c(1, 3, NA, 23, 3.0, 0)) %>%
#'   dplyr::group_by(file) %>%
#'   tidyr::nest()
#'
#' report_conversion_fails(df, columns = c("x", "y"), convert_to = "integer")
#' TODO:

report_conversion_fails <- function(df, columns = list(), convert_to = list()){


}
