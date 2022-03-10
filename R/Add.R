#' Add
#'
#' @param session BasexClient instance-ID
#' @param path Path
#' @param input Additional input (optional)
#'
#' @return A list with two items
#'     \itemize{
#'       \item {info} {Aditional info}
#'       \item {success} {Boolean, indicating if the command was completed successfull}
#'     }
#'
#' @description Adds a new resource to the opened database.
#'
#' @details The input can be a UTF-8 encoded XML document, a binary resource, or any other data (such as JSON or CSV)
#'     that can be successfully converted to a resource by the server.
#'     The utility-function \emph{input_to_raw} can be used to convert an arbitrary
#'     character vector to a stream.
#'     This method returns \emph{self} invisibly, thus making it possible to chain together multiple method calls.
#'
#' @examples
#' \dontrun{
#' Add(Session, "test", "<xml>Add</xml>")
#' }
#'
#' @export
Add <- function(session, path, input) {
  return(session$Add(path, input))
}
