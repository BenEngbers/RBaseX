#' GetSuccess
#'
#' @description Current value from session$Success
#' @param session BasexClient instance-ID
#'
#' @return Current value
#'
#' @export
GetSuccess <- function(session) {
  return(session$get_success())
}
