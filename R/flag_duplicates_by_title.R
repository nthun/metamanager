#' Flag duplicates by title
#'
#' Flags articles in a dataframe that are duplicated and has the same identifier
#' @name flag_duplicates_by_title
#' @usage flag_duplicates_by_title(df, title = NULL, max_distance = 5L, ...)
#'
#' @param df a data frame with potential duplicates
#' @param title the <chr> column in df  that has the title of studies
#' @param max_distance the maximum difference of titles to be flagged as duplicate
#'
#' @return The original data frame augmented with "duplicate_by_title" column, that can be 0 or 1
#' @details The function uses Optimal String Alignment distance to find the difference between strings, using the stringdist::stringdist() function (for details, see \code{\link[stringdist]{stringdist-metrics}}). Note that this function can also be used to find duplicates based on the abstract or any other text field. It can be a computationally heavy task for more and longer strings.
#' @seealso \code{\link{flag_duplicates_by_ids}} for flagging by ids
#' @examples
#' flag_duplicates_by_title(df, "title", max_distance = 5)

flag_duplicates_by_title <- function(df = NULL,
                                     title = "title",
                                     max_distance = 5L){

    stopifnot(is.data.frame(df),
              is.character(title),
              rlang::has_name(df, title),
              is.numeric(max_distance))

    df %>%
        # Create all combinations of titles
        tidystringdist::tidy_comb_all(!!rlang::sym(title)) %>%
        # Calculate string distances using OSA
        tidystringdist::tidy_stringdist(method = "osa") %>%
        dplyr::select(title1 = 1, title2 = 2, distance = 3) %>%
        # Keep only similar titles
        dplyr::filter(distance <= max_distance) %>%
        dplyr::transmute(!!title := title2,
                         duplicate_by_title = 1L) %>%
        # Make sure that each title is only present once, so no new duplicates are created
        distinct(!!rlang::sym(title), .keep_all = TRUE) %>%
        # Join to original data frame
        dplyr::left_join(df, ., by = title) %>%
        # Fill NA with 0-s
        dplyr::mutate(duplicate_by_title = dplyr::if_else(is.na(duplicate_by_title),
                                                          0L,
                                                          duplicate_by_title)) %>%
        # Make duplicates more identifiable by arranging by title
        dplyr::arrange(!!rlang::sym(title))
}

