#' Create
#'
#' @param session BasexClient instance-ID
#' @param name Database name
#' @param input Additional input, may be empty
#'
#' @return A list with two items
#'     \itemize{
#'       \item {info} {Aditional info}
#'       \item {success} {A boolean, indicating if the command was completed successfull}
#'     }
#'
#' @description Creates a new database with the specified name and input (may be empty).
#'
#' @details Initial content can be offered as string, URL or file.
#'  	'Check' is a convenience command that combines OPEN and CREATE DB: If a database
#'  	with the name input exists, and if there is no existing file or directory with the
#'  	same name that has a newer timestamp, the database is opened. Otherwise, a new
#'  	database is created; if the specified input points to an existing resource,
#'  	it is stored as initial content.
#'
#' @examples
#' \dontrun{
#' Create(, "test", "<xml>Create test</xml>")
#' Execute(Session, "Check test")
#' Create(Session, "test2",
#'   "https://raw.githubusercontent.com/BaseXdb/basex/master/basex-api/src/test/resources/first.xml")
#' Create(Session, "test3", "/home/username/Test.xml")
#' }
#'
#' @export
Create <- function(session, name, input) {
  return(session$Create(name, input))
}
