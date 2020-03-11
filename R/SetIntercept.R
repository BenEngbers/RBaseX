#' SetIntercept
#'
#' @description Assign a new value to session$Intercept
#'
#' @param session BasexClient instance-ID
#' @param intercept New Intercept value
#'
#' @examples
#' \dontrun{
#' SetIntercept(TRUE)
#' }
#'
#' @export
SetIntercept <- function(session, intercept) {
  return(session$set_intercept(intercept))
}
