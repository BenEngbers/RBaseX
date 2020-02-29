#' RestoreIntercept
#'
#' @param session BasexClient instance-ID
#'
#' @description Restore Intercept to original new value
#'
#' @export
RestoreIntercept <- function(session) {
  return(session$restore_intercept())
}
