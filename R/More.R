#' More
#'
#' @param query_obj QueryClass instance-ID
#'
#' @return Boolean
#'
#' @description Indicates if there are any other results in the query-result.
#'
#' @examples
#' \dontrun{
#' query_iterate <- Query(Session, "collection('TestDB/Test.xml')")
#' while (More(query_iterate)) {
#'     iterResult <- c(iterResult, Next(query_iterate))
#'   }
#'
#' print(query_iterate)
#' ## Return "0d" "<Line_1 line=\"1\">Content 1</Line_1>"
#'           "0d" "<Line_2 line=\"2\">Content 2</Line_2>"
#'
#' }
#'
#' @export
More <- function(query_obj) {
  return(query_obj$query$More())
}
