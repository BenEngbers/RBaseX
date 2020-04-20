#' Close
#'
#' @param query_obj QueryClass instance-ID
#'
#' @return NULL
#'
#' @description Closes and unregisters the query with the specified ID
#'
#' @details This method returns \emph{self} invisibly, thus making it possible to chain together multiple method calls.
#'
#' @export
Close <- function(query_obj) {
  return(query_obj$queryObject$Close())
}
