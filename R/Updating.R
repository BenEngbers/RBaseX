#' Updating
#'
#' @param query_obj Query instance-ID
#'
#' @return This function returns a list with the following items:
#'     \itemize{
#'       \item {result} {Result}
#'       \item {success} {A boolean, indicating if the command was completed successfull}
#'     }
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
