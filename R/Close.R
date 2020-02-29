#' Close
#'
#' @param query_obj QueryClass instance-ID
#'
#' @return NULL
#'
#' @description Closes and unregisters the query with the specified ID
#'
#' @export
Close <- function(query_obj) {
  return(query_obj$queryObject$Close())
}
