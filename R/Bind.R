#' Bind
#'
#' @param query_obj QueryClass instance-ID
#' @param ... Binding Information
#'
#' @return NULL
#'
#' @description Binds a value to a variable.
#'
#' @details Binding information can be provided in the following ways:
#'     \itemize{
#'       \item{name, value} Name and value for a variable.
#'       \item{name, value, type} Name, value and type for a variable.
#'     }
#'     For a list of possibe types see \url{http://docs.basex.org/wiki/Java_Bindings#Data_Types}
#'
#' @examples
#' \dontrun{
#' query_txt <- "declare variable $name external; for $i in 1 to 3 return element { $name } { $i }"
#' query_obj <- Query(Session, query_txt)
#' Bind(query_obj, "$name", "number")
#' print(Execute(query_obj))
#' }
#'
#' @export
Bind <- function(query_obj, ...) {
  return(query_obj$queryObject$Bind(...))
}
