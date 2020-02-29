#' Options
#'
#' @param query_obj QueryClass instance-ID
#'
#' @description Returns a string with all query serialization parameters, which
#'     can be assigned to the serializer option.
#'
#' @details For a list of possibe types see \url{http://docs.basex.org/wiki/Java_Bindings#Data_Types}
#'
#' @export
Options <- function(query_obj) {
  return(query_obj$queryObject$Options())
}
