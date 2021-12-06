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
#' Query_1 <- Query(Session, "collection('/TestDB/Test.xml')")
#' iterResult <- c()
#'
#' while (More(Query_1)) {
#'   iterResult <- c(iterResult, Next(Query_1))
#'   }
#'
#' print(iterResult)
#'
#'   [[1]]
#'   [1] "0d"                                    "<Line_1 line=\"1\">Content 1</Line_1>"
#'
#'   [[2]]
#'   [1] "0d"                                    "<Line_2 line=\"2\">Content 2</Line_2>"
#'
#' }
#'
#' @export
More <- function(query_obj) {
  return(query_obj$query$More())
}
