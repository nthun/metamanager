#' Tidy function for Krippendorff's alpha
#'
#' Creates a tidy data frame from Krippendorff's alpha list output
#' @name tidy_kripp
#' @usage tidy_kripp(kripp)
#' @param kripp a list object produced by irr::kripp.alpha()
#' @return A tibble containg the values of the Krippendorf's alpha output
#' @examples
#' tidy_kripp(screener_12_kripp)

tidy_kripp <- function(kripp){
    stopifnot(is.list(kripp),
              rlang::has_name(kripp, c("method","data.level","raters","subjects","value")))

    tibble::tibble(
        method = kripp$method,
        level = kripp$data.level,
        raters = kripp$raters,
        items = kripp$subjects,
        value = kripp$value
    )
}
