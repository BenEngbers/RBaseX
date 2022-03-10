#' Store
#'
#' @param session BasexClient instance-ID
#' @param path Path where to store the data
#' @param input Additional input, may be empty
#'
#' @return A list with two items
#'     \itemize{
#'       \item {info} {Aditional info}
#'       \item {success} {A boolean, indicating if the command was completed successfull}
#'     }
#'
#' @description Stores a binary resource in the opened database.
#'
#' @details Use the database-command \emph{retrieve} to retrieve the resource.
#'     The input can be a UTF-8 encoded XML document, a binary resource, or any other data (such as JSON or CSV)
#'     that can be successfully converted to a resource by the server.

#'     This method returns \emph{self} invisibly, thus making it possible to chain together multiple method calls.
#'
#' @examples
#' \dontrun{
#' Execute(Session, "DROP DB BinBase")
#' testBin <- Execute(Session, "Check BinBase")
#' bais <- raw()
#' for (b in 252:255) bais <- c(bais, c(b)) %>% as.raw()
#' test <- Store(Session, "test.bin", bais)
#' print(test$success)
#' baos <- Execute(Session, "retrieve test.bin")
#' print(bais)
#' print(baos$result)
#' }
#'
#' @export
Store <- function(session, path, input) {
  return(session$Store(path, input))
}
