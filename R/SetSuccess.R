#' SetSuccess
#'
#' @description Assign a new value to session$Success
#'
#' @param session BasexClient instance-ID
#' @param success Success-indicator for the last operation on the socket
#'
#' @examples
#' \dontrun{
#' SetSuccess(TRUE)
#' }
#'
#' @export
SetSuccess <- function(session, success) {
  return(session$set_success(success))
}
