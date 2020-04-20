#' RestoreIntercept
#'
#' @param session BasexClient instance-ID
#'
#' @description Restore Intercept to original new value
#'
#' @details This method returns \emph{self} invisibly, thus making it possible to chain together multiple method calls.
#'
#' @export
RestoreIntercept <- function(session) {
  return(session$restore_intercept())
}
