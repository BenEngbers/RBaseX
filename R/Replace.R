#' Replace
#'
#' @param session BasexClient instance-ID
#' @param path Path where to store the data
#' @param input Replacement
#'
#' @return A list with two items
#'     \itemize{
#'       \item {info} {Aditional info}
#'       \item {success} {A boolean, indicating if the command was completed successfull}
#'     }
#'
#' @description Replaces a resource with the specified input.
#'
#' @details The input can be a UTF-8 encoded XML document, a binary resource, or any other data (such as JSON or CSV)
#'     that can be successfully converted to a resource by the server.
#'     This method returns \emph{self} invisibly, thus making it possible to chain together multiple method calls.
#'
#' @examples
#' \dontrun{
#' Replace(Session, "test", "<xml>Create test</xml>")
#' }
#'
#' @export
Replace <- function(session, path, input) {
  return(session$Replace(path, input))
}
