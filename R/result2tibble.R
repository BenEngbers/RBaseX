#' result2tibble
#'
#' @return Return result from query as tibble
#'
#' @param input Query-result
#' @param cols Number of columns
#'
#' @description Converts the query-result to a tibble. The query-result is a list.
#'     'cols' is needed to determine the number of columns.
#'
#' @export
result2tibble <- function(input, cols) {
  return(as_tibble(result2matrix(input, cols)))
}
