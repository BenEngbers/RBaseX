#' result2tibble
#'
#' @return Return result from query as tibble
#'
#' @param ... Query-result
#'
#' @description Converts the query-result to a tibble. The query-result is either a list (sequence) or an array.
#'     If it is a list, 'cols' is needed to determine the number of columns.
#'
#' @export

result2tibble <- function(...) {
  input <- list(...)[[1]][[1]]
  if(length(list(...)) == 2) {    # Create frame from sequence
    cols <- list(...)[[2]]
    ma <- matrix(input, ncol=cols, byrow=TRUE)
    colnames(ma) <- colnames(ma, do.NULL = FALSE, prefix = "X")
    res_frame <- as.data.frame(ma, stringsAsFactors = FALSE)
    res_frame <- suppressWarnings(adj_coltype(res_frame, input[1:cols]))
    return(as_tibble(res_frame)) }
  else {
    template <- input[[1]] %>% str_replace_all("[\\[\\]]", "") %>% str_replace_all(", ", ",") %>%
      str_replace_all("\"", "'") %>% strsplit(",") %>% .[[1]]
    input <- input %>% str_replace_all("[\\[\\]]", "") %>% str_replace_all(", ", ",") %>% strsplit(",")

    num_cols <- length(template)
    num_rows <- length(input)
    res_frame <- data.frame(matrix(ncol = num_cols, nrow = 0))
      for (i in 1:num_rows) {  res_frame <- rbindlist(list(res_frame, as.list(input[[i]]))) }
    res_frame %<>% as.data.frame()
    res_frame <- suppressWarnings(adj_coltype(res_frame, template))
    return(as_tibble(res_frame))
  }
}
