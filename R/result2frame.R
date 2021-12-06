#' result2frame
#'
#' @return Return result from query as dataframe
#'
#' @param ... Query-result
#'
#' @description Converts the query-result to a frame. The query-result is either a list (sequence) or an array.
#'     If it is a list, 'cols' is needed to determine the number of columns.
#'
#' @export

result2frame <- function(...) {
  input <- list(...)[[1]][[1]]
  if(length(list(...)) == 2) {    # Create frame from sequence
    cols <- list(...)[[2]]
    ma <- matrix(input, ncol=cols, byrow=TRUE)
    colnames(ma) <- colnames(ma, do.NULL = FALSE, prefix = "X")
    res_frame <- as.data.frame(ma, stringsAsFactors = FALSE)
    res_frame <- suppressWarnings(adj_coltype(res_frame, input[1:cols]))
    return(res_frame)
  } else {                        # Create frame from array
    template <- input[[1]] %>% str_replace_all("[\\[\\]]", "") %>% str_replace_all(", ", ",") %>%
      str_replace_all("\"", "'") %>% strsplit(",") %>% .[[1]]
    input <- input %>% str_replace_all("[\\[\\]]", "") %>% str_replace_all(", ", ",") %>% strsplit(",")

    num_cols <- length(template)
    num_rows <- length(input)
    res_frame <- data.frame(matrix(ncol = num_cols, nrow = 0))

    for (i in 1:num_rows) {  res_frame <- rbindlist(list(res_frame, as.list(input[[i]]))) }
    res_frame %<>% as.data.frame()
    res_frame <- suppressWarnings(adj_coltype(res_frame, template))
    return(res_frame)
  }
}

adj_coltype <- function(frame_in, template) {
  frame_out <- frame_in
  Bools <- which(frame_in[1,] %in% c("true", "true()", "false", "false()"))
  NonBools <- setdiff(1:length(template), Bools)
  for (i in Bools) {
    Ts <- which(frame_in[,i] %in%  c("true", "true()"))
    frame_out[,i] <- as.logical(frame_in[,i])
    frame_out[,i][-Ts] <- FALSE
    frame_out[,i][Ts]  <- TRUE
    frame_out[,i] <- as.logical(frame_out[,i])
  }
  for (i in NonBools) {
    num <- as.numeric(template[i])
    if (!is.na(num) && num %% 1 != 0)      { frame_out[,i] <- as.numeric(frame_out[,i]) }
    else if (!is.na(num) && num %% 1 == 0) { frame_out[,i] <- as.integer(frame_out[,i]) }
  }
  return(frame_out)
}
