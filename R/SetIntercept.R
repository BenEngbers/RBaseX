#' SetIntercept
#'
#' @param session BasexClient instance-ID
#' @param intercept New Intercept value
#'
#' @description Assign a new value to session$Intercept
#'
#' @details This method returns \emph{self} invisibly, thus making it possible to chain together multiple method calls.
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
