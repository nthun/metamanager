#' Flag duplicates by title
#'
#' Flags articles in a dataframe that are duplicated and has the same identifier
#' @name flag_duplicate_title
#' @usage flag_duplicate_title(df, title = "title", max_distance = 5L)
#'
#' @param df a data frame with potential duplicates
#' @param title the <chr> column in df  that has the title of studies
#' @param max_distance the maximum difference of titles to be flagged as duplicate
#'
#' @return The original data frame augmented with "duplicate_by_title" column, that can be 0 or 1
#' @details The function uses Optimal String Alignment distance to find the difference between strings, using the stringdist::stringdist() function (for details, see \code{\link[stringdist]{stringdist-metrics}}). Note that this function can also be used to find duplicates based on the abstract or any other text field. It can be a computationally heavy task for more and longer strings.
#' @importFrom tidystringdist tidy_comb_all tidy_stringdist
#' @importFrom rlang !! sym has_name
#' @importFrom dplyr distinct if_else arrange mutate select filter transmute left_join
#' @export
#'
#' @examples
#' library(dplyr)
#' # Show all articles with duplicated title
#' merge_sources(workaholism_pubmed, workaholism_psychinfo) %>%
#'  make_id(c("psyid", "pmid", "doi", "eid", "sid")) %>%
#'  flag_duplicate_title(title = "title") %>%
#'  filter(duplicate_by_title == 1)

flag_duplicate_title <- function(df,
                                 title = "title",
                                 max_distance = 5){

    stopifnot(is.data.frame(df),
              is.character(title),
              has_name(df, title),
              is.numeric(max_distance))

    df %>%
        # Create all combinations of titles
        tidy_comb_all(!!rlang::sym(title)) %>%
        # Calculate string distances using OSA
        tidy_stringdist(method = "osa") %>%
        select(title1 = 1, title2 = 2, distance = 3) %>%
        # Keep only similar titles
        filter(distance <= max_distance) %>%
        transmute(!!title := title2,
                         duplicate_by_title = 1L) %>%
        # Make sure that each title is only present once, so no new duplicates are created
        distinct(!!sym(title), .keep_all = TRUE) %>%
        # Join to original data frame
        left_join(df, ., by = title) %>%
        # Fill NA with 0-s
        mutate(duplicate_by_title = if_else(is.na(duplicate_by_title),
                                            0L,
                                            duplicate_by_title)) %>%
        # Make duplicates more identifiable by arranging by title
        arrange(!!sym(title))
}

