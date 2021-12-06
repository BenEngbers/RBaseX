#' Info
#'
#' @param query_obj QueryClass instance-ID
#'
#' @return This function returns a list with the following items:
#'     \itemize{
#'       \item {Info} {Info}
#'       \item {success} {A boolean, indicating if the command was completed successfull}
#'     }
#'
#' @description Returns a string with query compilation and profiling info.
#'
#' @details If the query object has not been executed yet, an empty string is returned.
#'
#' @export
Info <- function(query_obj) {
  return(query_obj$queryObject$Info())
}
