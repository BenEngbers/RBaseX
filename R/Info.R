#' Info
#'
#' @param query_obj QueryClass instance-ID
#'
#' @description Returns a string with query compilation and profiling info.
#'
#' @details If the query object has not been executed yet, an empty string is returned.
#'
#' @export
Info <- function(query_obj) {
  return(query_obj$queryObject$Info())
}
