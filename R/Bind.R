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
#'       \item{name, list(value)} Name, list of values.
#'       \item{name, list(value), list(type)} Name, list of values, list of types.
#'     }
#'     For a list of possibe types see \url{http://docs.basex.org/wiki/Java_Bindings#Data_Types}
#'
#'     This method returns \emph{self} invisibly, thus making it possible to chain together multiple method calls.
#'
#' @examples
#' \dontrun{
#' query_obj <- Query(Session,
#'   "declare variable $name external; for $i in 1 to 2 return element { $name } { $i }")
#' Bind(query_obj, "$name", "number")
#' print(Execute(query_obj))
#'
#' query_obj <- Query(Session,
#'   "declare variable $name external; for $i in 3 to 4 return element { $name } { $i }")
#' Bind(query_obj, "$name", "number", "xs:string")
#' print(Execute(query_obj))
#'
#' query_obj <- Query(Session,
#'   "declare variable $name external;
#'   for $t in collection('TestDB/Books')/book where $t/@author = $name
#'   return $t/@title/string()")
#' Bind(query_obj, "$name", list("Walmsley", "Wickham"))
#' print(Execute(query_obj))
#'
#' query_obj <- Query(Session,
#'   "declare variable $name external;
#'   for $t in collection('TestDB/Books')/book where $t/@author = $name
#'   return $t/@title/string()")
#' Bind(query_obj, "$name", list("Walmsley", "Wickham"), list("xs:string", "xs:string"))
#' print(Execute(query_obj))
#' }
#'
#' @export
Bind <- function(query_obj, ...) {
  return(query_obj$queryObject$Bind(...))
}
