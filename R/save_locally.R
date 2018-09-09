#' Save several files locally based on a nesting variable
#'
#' Cut a dataframe into several pieces based on a nesting variable, and save these pieces separately in a specified folder with a specified postfix.
#' @name save_locally
#' @usage save_locally(df, local_path, nesting, postfix, overwrite)
#' @param df a data frame that should be saved in segments
#' @param local_path a full path <chr> where the files will be saved as .csv
#' @param nesting variable name <chr> that will used as nesting variable
#' @param postfix a string <chr> that will be appended to the end of the file name
#' @param overwrite overwrite files <lgl>? (default = FALSE)
#' @return No output, this function exerts a side-effect.
#' @examples
#' \dontrun{
#' save_locally(articles,
#'              "d:/temp/screening/",
#'              nesting = "reviewer",
#'              postfix = "",
#'              overwrite = TRUE)
#' }
# TODO: test!
# TODO: Make it tidyeval

save_locally <-
    function(df,
             local_path = NULL,
             nesting = NULL,
             postfix = NULL,
             overwrite = FALSE) {

    # Error handling
    stopifnot(
        !is.null(local_path),
        !is.null(nesting),
        !is.null(postfix),
        rlang::has_name(df, nesting),
        !(dir.exists(local_path) & overwrite == FALSE))

    # Create a nested tibble
    df_nested <-
        df %>%
        dplyr::group_by(!!rlang::sym(nesting)) %>%
        tidyr::nest()

    # Create the directory
    if (dir.exists(local_path) & overwrite == TRUE) unlink(local_path, recursive = TRUE)
    dir.create(local_path)

    # Save the screening files
    purrr::walk2(dplyr::pull(df_nested, !!rlang::sym(nesting)),
                 dplyr::pull(df_nested, data),
                 ~readr::write_excel_csv(x = .y,
                                         path = stringr::str_glue("{local_path}/{.x}_{postfix}.csv"), na = ""))
    message(stringr::str_glue("{nrow(df_nested)} files saved in {local_path}"))
}
