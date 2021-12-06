#' Options
#'
#' @param query_obj QueryClass instance-ID
#'
#' @return This function returns a list with the following items:
#'     \itemize{
#'       \item {Options} {Options}
#'       \item {success} {A boolean, indicating if the command was completed successfull}
#'     }
#'
#' @description Returns a string with all query serialization parameters, which
#'     can be assigned to the serializer option.
#'
#' @details For a list of possibe types see \url{https://docs.basex.org/wiki/Java_Bindings#Data_Types}
#'
#' @export
Options <- function(query_obj) {
  return(query_obj$queryObject$Options())
}
