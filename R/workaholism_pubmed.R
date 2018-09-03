#' Workaholism articles from PubMed
#'
#' Article metadata returned for workaholism keywords by PubMed.
#' Search expression to obtain data: ((("work addict*"[Title/Abstract] OR "work depend*"[Title/Abstract] OR workaholism[Title/Abstract]) AND English[lang] AND (Journal Article[ptyp] OR pubmed books[filter])))
#' The search was conducted on 2018/04/02
#'
#' @format
#' \describe{
#'   \item{pmid}{Pubmed id}
#'   \item{doi}{Digital object identifier}
#'   \item{title}{Title of the article}
#'   \item{abstract}{Abstract of the article}
#'   \item{year}{Year of publication}
#'   \item{journal}{Name of the journal}
#'   \item{authors}{Name of all authors}
#'   }
"workaholism_pubmed"
