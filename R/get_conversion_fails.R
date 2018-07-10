#' Checks for coversion errors in nested dataframes
#'
#' Checks if a specific conversion produces any errors in the nested dfs.
#' @name get_conversion_fails
#' @usage get_conversion_fails(df, columns, convert_to)
#'
#' @param df data frame of nested data frames
#' @param columns a vector of column names <chr> to be converted
#' @param convert_to output variable type can be "integer", "logical", "double" (character and factor conversion does not produce errors)
#'
#' @return A tibble that shows all conversion errors for a type for each file
#' @examples
#'
#' df <-
#'   tibble::tibble(file = c("file1","file1", "file2", "file2", "file2"),
#'                  x = c(1,2,"c", 4, "d"),
#'                  y = c(1, 3, NA, 23, 3.0, 0)) %>%
#'   dplyr::group_by(file) %>%
#'   tidyr::nest()
#'
#' get_conversion_fails(df, columns = c("x", "y"), convert_to = "integer")
#' TODO: BUG: too much data returned if only one column is provided for conversion

get_conversion_fails <- function(df, columns = NULL, convert_to = c("integer","double","logical")){

    # Error handling
    stopifnot(is.data.frame(df),
              is.character(columns),
              length(columns) > 0,
              convert_to %in% c("integer", "double", "logical"),
              length(convert_to) == 1,
              rlang::has_name(df, columns))

    # Use only the first argument of convert_to, and make it a function name
    fun <- match.fun(paste0("as_", convert_to[1]))

    df %>%
        mutate(
               # Create a nested with cell-wise conversion warnings
               conversion_errors = purrr::map(sheet,
                                                ~dplyr::select(.x, dplyr::one_of(columns)) %>%
                                                    get_cellwise_conversion_fails(., fun = !!fun)),
               conversion_sums = purrr::map(conversion_errors,
                                            ~dplyr::summarise_all(.x, ~sum(!is.na(.))))) %>%
        transmute(file,
                  fun = convert_to,
                  report = purrr::map(conversion_sums,
                                    ~tidyr::gather(.x, variable, value) %>%
                                     dplyr::group_by(variable) %>%
                                     dplyr::summarise(conversion_fails = mean(value)) %>%
                                     dplyr::filter(conversion_fails != 0))) %>%
        tidyr::unnest(report)
}
