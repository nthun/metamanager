#' Cell-wise warnings for conversion failiures
#'
#' Helper function to get conversion warnings separately for each cell
#' @name get_cellwise_conversion_fails
#' @usage get_cellwise_conversion_fails(df, fun)
#'
#' @param df data frame
#' @param fun unquoted conversion function name without ()
#'
#' @return A data frame, where cells contain either NA (no problem with conversion), or the warning message as string
#' @examples
#' df <- tibble::tibble( a = as.character(1:5),
#'                       b = as.double(1:5),
#'                       c = as.character(c(1:2, letters[1:3])),
#'                       d = as.character(c(letters[4:5], c(3:5))))
#' get_cellwise_conversion_fails(df, rlang::as_integer)

get_cellwise_conversion_fails <- function(df, fun){

    # Error handling
    stopifnot(is.data.frame(df),
              is.function(fun))
    # Make a quiet version of the function
    quiet_fun <- purrr::safely(fun)

    # Create a cell-wise report of conversion errors
    df %>%
        # This following part works but may not be optimal. Improve if possible!
        purrr::map(., ~purrr::map(.x, ~quiet_fun(.x)) %>%
                       purrr::transpose() %>%
                       .[["error"]] %>%
                       as.character()
        ) %>%
        tibble::as_data_frame() %>%
        # This seems suboptimal, should render empty vectors as NA earlier (but how?)
        dplyr::mutate_all(dplyr::funs(dplyr::recode), `character(0)` = NA_character_)
}
