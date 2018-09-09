#' Checks for coversion errors in a dataframe
#'
#' Checks if a specific conversion produces any errors in a df.
#' @name get_conversion_fails
#' @usage get_conversion_fails(df, columns = NULL, convert_to = c("integer","double","logical"))
#'
#' @param df data frame
#' @param columns a vector of column names <chr> to be converted
#' @param convert_to output variable type can be "integer", "logical", "double" (character and factor conversion does not produce errors)
#'
#' @return A tibble that shows all conversion errors for a type
#' @examples
#'
#'df <-
#'  tibble::tibble(file = c("file1", "file1", "file2", "file2", "file2"),
#'                 x = c(1,2,"c", 4, "d"),
#'                 y = c(1, 3, NA, 23, 3.0)) %>%
#'  dplyr::group_by(file) %>%
#'  tidyr::nest()
#'
#'get_conversion_fails(df,
#'                     columns = c("x", "y"),
#'                     convert_to = "integer")
#'
#' TODO: change example

# get_conversion_fails <- function(df,
#                                  columns = NULL,
#                                  convert_to = c("integer","double","logical")){
#
#     # Error handling
#     stopifnot(is.data.frame(df),
#               is.character(columns),
#               length(columns) > 0,
#               convert_to %in% c("integer", "double", "logical")
#               )
#
#     fun <- match.fun(paste0("as_", convert_to[1]))
#
#         # Get conversion fails
#     df %>%
#         dplyr::select(variables_to_convert) %>%
#         get_cellwise_conversion_fails(., fun = fun) %>%
#         dplyr::summarise_all(., ~sum(!is.na(.))) %>%
#         tidyr::gather(variable, conversion_fails) %>%
#         dplyr::filter(conversion_fails != 0) %>%
#         dplyr::arrange(-conversion_fails)
# }
