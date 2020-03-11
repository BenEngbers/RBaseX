#' Title Full
#'
#' @param query_obj QueryClass instance-ID
#'
#' @description Executes a query and returns a vector with all resulting items as strings,
#'     prefixed by the 'XDM' (Xpath Data Model) Meta Data <https://www.xdm.org/>.
#'     Meta Data and results are seaparated by a '|'.
#'
#' @examples
#' \dontrun{
#' query_txt <- "collection('TestDB/Test.xml')"
#' query_obj <- Query(Session, query_txt)
#'
#' print(Full(query_obj))
#' ## Return "0d" "/TestDB/Test.xml <Line_1 line=\"1\">Content 1</Line_1>"
#'           "0d" "/TestDB/Test.xml <Line_2 line=\"2\">Content 2</Line_2>"
#'
#' }
#'
#' @export
Full <- function(query_obj) {
  return(query_obj$queryObject$Full())
}
