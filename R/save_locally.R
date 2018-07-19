#' Save several files locally based on a nesting variable
#'
#' Cut a dataframe into several pieces based on a nesting variable, and save these pieces separately in a specified folder with a specified postfix.
#' @name save_locally
#' @usage save_locally(df, local_path = NULL, nesting = NULL, postfix = NULL, overwrite = FALSE)
#' @param df a full path <chr> that contains .csv-s to upload to gdrive
#' @param local_path a full path <chr> where the files will be saved as .csv
#' @param nesting variable name <chr> that will used as nesting variable
#' @param postfix a string <chr> that will be appended to the end of the file name
#' @param overwrite overwrite files <lgl>? (default = FALSE)
#' @return no output, this function exerts a side-effect
#' @examples
#' save_locally(articles, "d:/temp/screening/", nesting = "reviewer", postfix = "",overwrite = TRUE)
# TODO: test!
# TODO: nesting variable does not work well. Should come up with something better.
# TODO: Make it tidyeval
# TODO: ? use fs instead of base file operations

save_locally <- function(df, local_path = NULL, nesting = NULL, postfix = NULL, overwrite = FALSE){
    # Checking predicaments
    stopifnot(
                !is.null(dir),
                !is.null(nesting),
                !is.null(postfix),
                rlang::has_name(df, !!nesting),
                !(dir.exists(dir) & overwrite == FALSE))
    # Create a nested tibble
    df_nested <-
        df %>%
        dplyr::group_by(!!nesting) %>%
        tidyr::nest()
    # Create the directory
    if (dir.exists(dir) & overwrite == TRUE) unlink(dir, recursive = TRUE)
    dir.create(dir)
    # Save the screening files
    purrr::walk2(df_nested[, nesting], df_nested[, "data"], ~readr::write_excel_csv(.y, stringr::str_glue("{dir}/{.x}_{postfix}.csv"), na = ""))
    stringr::str_glue("{nrow(df_nested)} files saved in {dir}")
}

