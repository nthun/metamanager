#' Flag duplicates by title
#'
#' Flags articles in a dataframe that are duplicated and has the same identifier
#' @name flag_duplicates_by_title
#' @usage flag_duplicates_by_title(df, title = NULL, max_distance = 4L, ...)
#'
#' @param df a data frame with potential duplicates
#' @param title the <chr> column in df  that has the title of studies
#' @param max_distance the maximum difference of titles to be flagged as duplicate
#' @param ... parameters to be passed to stringdist::stringdistmatrix()
#'
#' @return The original data frame augmented with "duplicate_by_title" column, that can be 0 or 1
#' @details Note that this function can also be used to find duplicates based on the abstract or any other unique text field. However, this might prove to be slow due to the string distance calculations.
#' @seealso \code{\link{flag_duplicates_by_ids}} for flagging by ids
#' @examples
#' flag_duplicates_by_title(df, "title", max_distance = 3)

flag_duplicates_by_title <- function(df, title = NULL, max_distance = 4L, ...){

    stopifnot(is.data.frame(df),
              is.character(title),
              rlang::has_name(df, title),
              is.numeric(max_distance))

    # Create a distance matrix with the title variable
    dist_matrix <-
        df %>%
        dplyr::pull(!!title) %>%
        # stringdistmatrix uses multiple threads by default, and can take a lot of time
        # Default method for string matching is "osa", which imo works fine for article titles, but somewhat slow
        stringdist::stringdistmatrix(useNames = "strings", ...) %>%
        as.matrix()

    # Remove diagonal and upper triangle to get rid of duplicates
    # I wonder if this could be done in a simpler way (like a parameter)
    diag(dist_matrix) <- NA
    dist_matrix[lower.tri(dist_matrix)] <- NA

    dist_matrix %>%
        # Keep rownames and presearve titles as they are
        tibble::as_tibble(rownames = NA) %>%
        tibble::rownames_to_column("title1") %>%
        # Make relational data
        tidyr::gather(title2, distance, - title1) %>%
        # Keep only similar titles
        dplyr::filter(distance <= max_distance) %>%
        dplyr::transmute(!!title := title2,
                         duplicate_by_title = 1L) %>%
        # Join to original data frame
        dplyr::left_join(df, ., by = title) %>%
        # Fill NA with 0-s
        dplyr::mutate(duplicate_by_title = dplyr::if_else(is.na(duplicate_by_title),
                                                          0L,
                                                          duplicate_by_title)) %>%
        # Make duplicates more identifiable by arranging by title
        dplyr::arrange(!!rlang::sym(title))
}

