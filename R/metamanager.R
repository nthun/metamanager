#' Functions for efficient meta-analysis project management
#'
#' Helps to menage files and workflow in meta-analysis projects
#'
#' @name metamanager-package
#' @aliases metamanager-package metamanager
#' @docType package
#' @author Tamas NAGY <nagytamas.hungary@gmail.com>
#' @importFrom dplyr %>%
#' @importFrom rlang :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
