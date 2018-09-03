#' Workaholism articles from Scopus
#'
#' Article metadata returned for workaholism keywords by Scopus
#' Search expression to obtain data: TITLE-ABS-KEY ( "work addict*"  OR  "work depend*"  OR  workaholism )  AND  ( LIMIT-TO ( LANGUAGE ,  "English" ) )  AND  ( LIMIT-TO ( DOCTYPE ,  "ar" )  OR  LIMIT-TO ( DOCTYPE ,  "ch" ) )
#' The search was conducted on 2018/01/16
#'
#' @format
#' \describe{
#'   \item{doi}{Digital object identifier}
#'   \item{eid}{Electronic identifier}
#'   \item{sid}{Yet another identifier}
#'   \item{pmid}{Pubmed id}
#'   \item{title}{Title of the article}
#'   \item{publication}{Name of the journal or other source}
#'   \item{authors}{Name of all authors}
#'   \item{year}{Year of publication}
#'   \item{month}{Month of publication}
#'   \item{day}{Day of publication}
#'   \item{abstract}{Abstract of the article}
#'   }
"workaholism_scopus"
