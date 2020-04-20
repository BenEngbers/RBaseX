#' Context
#'
#' @param query_obj QueryClass instance-ID
#' @param value Value that should be boud to the context
#' @param type The type will be ignored when the string is empty
#'
#' @description Binds a value to the context. The type will be ignored if the string is empty.
#'
#' @details  The type that is provided to the context, should be one of the standard-types.
#'     An alternative way is to parse the document information.
#'     This method returns \emph{self} invisibly, thus making it possible to chain together multiple method calls.
#'
#' @examples
#' \dontrun{
#' ctxt_query_txt <- "for $t in .//text() return string-length($t)"
#' ctxt_query     <- Query(Session, ctxt_query_txt)
#' ctxt_txt       <- paste0("<xml>",
#'                          "<txt>Hi</txt>",
#'                          "<txt>World</txt>",
#'                          "</xml>")
#' Context(ctxt_query, ctxt_txt, type = "document-node()")
#' print(Execute(ctxt_query))  ## returns "2"  "5"
#'
#' ctxt_query_txt <- "for $t in parse-xml(.)//text() return string-length($t)"
#' Context(ctxt_query, ctxt_txt)
#' print(Execute(ctxt_query))
#' }
#'
#' @export
Context <- function(query_obj, value, type) {
  if (missing(type)) type <- ""
  return(query_obj$queryObject$Context(value, type))
}
