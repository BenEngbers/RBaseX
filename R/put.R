#' Put
#'
#' @param session BasexClient instance-ID
#' @param path Path where to store the data
#' @param input Add or replacement
#'
#' @return A list with two items
#'     \itemize{
#'       \item {info} {Aditional info}
#'       \item {success} {A boolean, indicating if the command was completed successfull}
#'     }
#'
#' @description Adds or replaces a resource with the specified input.
#'
#' @details The input can be a UTF-8 encoded XML document, a binary resource, or any other data (such as JSON or CSV)
#'     that can be successfully converted to a resource by the server.
#'     This method returns \emph{self} invisibly, thus making it possible to chain together multiple method calls.
#'
#' @examples
#' \dontrun{
#' put(Session, "test", "<xml>Create test</xml>")
#' }
#'
#' @export
put <- function(session, path, input) {
  return(session$put(path, input))
}
