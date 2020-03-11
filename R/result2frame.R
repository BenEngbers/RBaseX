#' result2frame
#'
#' @return Return result from query as dataframe
#'
#' @param input Query-result
#' @param cols Number of columns
#'
#' @description Converts the query-result to a frame. The query-result is a list.
#'     'cols' is needed to determine the number of columns.
#'
#' @export
result2frame <- function(input, cols) {
  return(as.data.frame(result2matrix(input, cols)))
}
