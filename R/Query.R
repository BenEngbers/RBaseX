#' Query
#'
#' @param session BasexClient instance-ID
#' @param query_string query string
#'
#' @return Query_ID
#'
#' @description Creates a new query instance and returns its id.
#'
#' @examples
#' \dontrun{
#' query_txt <- "for $i in 1 to 2 return <xml>Text { $i }</xml>"
#' query_obj <- Query(Session, query_txt)
#' print(Execute(query_obj))
#' }
#'
#' @export
Query <- function(session, query_string) {
  if (missing(query_string)) {
    session$set_success(FALSE)
    if (session$get_intercept()) {
      return(list(queryObject = NULL, success = session$get_success()))
    } else stop("No query-string provided")
  }
  tryCatch(
    { queryObject <- QueryClass$new(query_string, session)
    success <- session$get_socket()$bool_test_sock()
    session$set_success(success)
    return(list(queryObject = queryObject, success = session$get_success()))
    },
    error = function(e) {
      success <- session$get_socket()$bool_test_sock()
      session$set_success(success)
      if (session$get_intercept()) {
        return(list(queryObject = NULL, success = session$get_success()))
      } else {
        message("Error creating the query-object")
        stop()}
    }
  )
}
