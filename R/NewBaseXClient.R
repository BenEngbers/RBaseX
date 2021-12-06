#' Title
#'
#' @param host,port Host name and port-number
#' @param user,password User credentials
#'
#' @return BasexClient-instance
#'
#' @description Create a BaseX-client
#'
#' @details This creates a BaseX-client. By default it listens to port 1984 on localhost.
#'     Username and password should be changed after the installation of 'BaseX'.
#'
#' @examples
#'
#' \dontrun{
#' session <- NewBasexClient(user = <username>, password = "<password>")
#' }
#'
#' @export
NewBasexClient <- function(host = "localhost", port = 1984, user, password) {
    Session <- BasexClient$new(host, port, user, password)
}
