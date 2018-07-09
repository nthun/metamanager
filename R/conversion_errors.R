#' Checks for coversion errors
#'
#' Checks if a specific conversion produces any errors in the nested dfs that has (partly) the same variable names.
#' @name conversion_errors
#' @usage conversion_errors(df, columns, convert_to)
#'
#' @param df nested data frame of files
#' @param columns a vector of column names <chr> to be converted
#' @param fun a conversion function e.g. as.integer
#'
#' @return A tibble that shows all conversion errors for each file
#' @examples
#'
#' df <-
#'   tibble::tibble(file = c("file1","file1", "file2", "file2", "file2"),
#'                  x = c(1,2,"c", 4, "d")) %>%
#'   dplyr::group_by(file) %>%
#'   tidyr::nest()
#'
#' conversion_errors(df, columns = c("x"), convert_to = "integer")

conversion_errors <- function(df, columns = NULL, convert_to = c("integer","numeric","double")){

    # Error handling
    stopifnot(is.data.frame(df),
              is.character(columns),
              length(columns) > 0,
              convert_to %in% c("integer", "double", "logical"),
              length(convert_to) == 1)

    # Use only the first argument of convert_to, and make it a function name
    fun <- match.fun(paste0("as_", convert_to[1]))

    df %>%
        mutate(file,
               fun = convert_to,
               # Create a nested with cell-wise conversion warnings
               conversion_warnings = purrr::map(sheet,
                                                ~dplyr::select(.x, dplyr::one_of(columns)) %>%
                                                 get_conversion_warnings(., fun = !!fun)),
               conversion_sums = purrr::map(conversion_warnings,
                                            ~dplyr::summarise_all(.x, ~sum(!is.na(.)))),
               report = purrr::map(conversion_sums,
                            ~tidyr::gather(.x, variable, value) %>%
                             dplyr::group_by(variable) %>%
                             dplyr::summarise(conversion_errors = mean(value)) %>%
                             dplyr::filter(conversion_errors != 0))) %>%
        tidyr::unnest(report)
}
