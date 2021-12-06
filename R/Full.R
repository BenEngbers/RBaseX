#' Title Full
#'
#' @param query_obj QueryClass instance-ID
#'
#' @description Executes a query and returns a list of vectors, each one representing a result as a string ,
#'     prefixed by the 'XDM' (Xpath Data Model) Meta Data <https://www.xdm.org/>.
#'     Meta Data and results are seaparated by a '|'.
#'
#' @examples
#' \dontrun{
#' query_txt <- "collection('/TestDB/Test.xml')"
#' query_obj <- Query(Session, query_txt)
#'
#' print(Full(query_obj))
#'
#' ## Return
#' [[1]]
#' [1] "2f"               "/TestDB/Test.xml"
#' [[2]]
#' [1] "3c"                                  "Line_1 line=\"1\">Content 1</Line_1"
#' [[3]]
#' [1] "2f"               "/TestDB/Test.xml"
#' [[4]]
#' [1] "3c"                                  "Line_2 line=\"2\">Content 2</Line_2"
#'
#' }
#'
#' @export
Full <- function(query_obj) {
  return(query_obj$queryObject$Full())
}
