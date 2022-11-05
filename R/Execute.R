#' Execute
#'
#' @param ... The command or query to be executed.
#'     When used to execute a command, a SessionID and a string which contains the command, are to be passed.
#'     When used to execute a query, the QueryClass instance-ID is passed.
#'
#' @return When used to execute commands in the Standard mode, this function returns a list with the following items:
#'     \itemize{
#'       \item {result}
#'       \item {info} {Aditional info}
#'       \item {success} {A boolean, indicating if the command was completed successfull}
#'     }
#'
#'     When used to execute a query, it return the result as a list.
#'
#' @description Executes a database command or a query.
#'
#' @details The 'Execute' command is deprecated and has been renamed to 'Command'.
#'     'Execute' is being kept as convenience.
#'
#' @examples
#' \dontrun{
#' Session <- NewBasexClient(user = <username>, password = "<password>")
#' print(Execute(Session, "info")$info)
#'
#' query_txt <- paste("for $i in 1 to 2", "return <xml>Text { $i }</xml>", sep = " ")
#' query_obj <- Query(Session, query_txt)
#' print(Execute(query_obj))
#' }
#'
#' @export
Execute <- function(...) {
  arguments <- list(...)
  classType <- unlist(class(arguments[[1]]))[1]
  if ( classType == "BasexClient") {
    session <- arguments[[1]]
    comm <- arguments[[2]]
    return(session$Execute(comm))
  } else {
    query_obj <- arguments[[1]][[1]]
    return(query_obj$ExecuteQuery())
  }
}
