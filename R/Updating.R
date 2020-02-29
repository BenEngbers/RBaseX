#' Updating
#'
#' @param query_obj Query instance-ID
#'
#' @return Boolean
#'
#' @description Check if the query contains updating expressions.
#'
#' @details Returns \emph{TRUE} if the query contains updating expressions;
#'     \emph{FALSE} otherwise.
#'
#' @export
Updating <- function(query_obj) {
  return(query_obj$queryObject$Updating())
}
