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
#' @examples
#' \dontrun{
#' Replace(Session, "test", "<xml>Create test</xml>")
#' }
#'
#' @export
Replace <- function(session, path, input) {
  return(session$Replace(path, input))
}
