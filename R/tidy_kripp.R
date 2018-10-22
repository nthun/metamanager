#' Tidy function for Krippendorff's alpha
#'
#' Creates a tidy data frame from Krippendorff's alpha list output
#' @name tidy_kripp
#' @usage tidy_kripp(kripp)
#' @param kripp a list object produced by irr::kripp.alpha()
#' @return A tibble containg the values of the Krippendorf's alpha output
#' @importFrom rlang has_name
#' @importFrom  dplyr tibble
#' @export
#'
#' @examples
#'

tidy_kripp <- function(kripp){
    stopifnot(is.list(kripp),
              has_name(kripp, c("method","data.level","raters","subjects","value")))

    tibble(
        all_items = kripp$subjects,
        raters = kripp$raters,
        method = kripp$method,
        kr_level = kripp$data.level,
        kr_alpha = kripp$value
    )
}
