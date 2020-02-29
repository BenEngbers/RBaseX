#' GetIntercept
#'
#' @description Current value for session$Intercept
#' @param session BasexClient instance-ID
#'
#' @return Current value
#'
#' @export
GetIntercept <- function(session) {
  return(session$get_intercept())
}
