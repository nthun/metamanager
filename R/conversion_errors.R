#' Checks for coversion errors
#'
#' Checks if a specific conversion produces any errors in the nested dfs that has (partly) the same variable names.
#' @name conversion_errors
#' @usage conversion_errors(df = NULL, columns = NULL, fun = NULL)
#'
#' @param df nested data frame of files
#' @param columns a vector of column names <chr> to be converted
#' @param fun a conversion function e.g. as.integer
#'
#' @return A tibble that shows all conversion errors for each file
#' @examples
#'
#' df <-
#'   tibble::tibble(file = c("file1","file1", "file2", "file2", "file2"), x = c(1,2,"c", 4, "d")) %>%
#'   group_by(file) %>%
#'   nest()
#'
#' conversion_errors(df, columns = c("x"), fun = as.integer)
# TODO: Make column selection tidyeval!
# TODO: error handling!
# TODO: This function should be a wrapper arounf get conversion watnings, with limited number of conversion functions, etc.
# TODO: Make fun column to be human readable

conversion_errors <- function(df, columns, fun){

df %>%
    mutate(file,
           fun = deparse(enquo(fun)),
           conversion_warnings = purrr::map(sheet, ~dplyr::select(.x, dplyr::one_of(columns)) %>%
                                                     get_conversion_warnings(., fun = !!fun)),
           conversion_sums = purrr::map(conversion_warnings, ~dplyr::summarise_all(.x, ~sum(!is.na(.)))),
           report = purrr::map(conversion_sums,
                        ~tidyr::gather(.x, variable, value) %>%
                         dplyr::group_by(variable) %>%
                         dplyr::summarise(conversion_errors = mean(value)) %>%
                         dplyr::filter(conversion_errors != 0))) %>%
    tidyr::unnest(report)
}
