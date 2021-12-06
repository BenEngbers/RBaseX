#' @title input_to_raw
#'
#' @return 'Raw' vector
#'
#' @param input Character vector length 1
#'
#' @description Convert \emph{input} to a length-1 character vector.
#'
#' @details If \emph{input} is a reference to a file, the number of bytes
#'     corresponding to the size is read. If it is an URL, the URL is read and converted to a 'Raw' vector.
#'     The function does not catch errors.
#'
#' @export

input_to_raw <- function(input) {
  type <- typeof(input)
  switch (type,
          "raw"       = raw_input <- input,       # Raw
          "character" = {
            if (input == "") {                    # Empty input
              raw_input <- raw(0)
            } else if (file.exists(input)) {      # File on filesystem
              finfo <- file.info(input)
              toread <- file(input, "rb")
              raw_input <- readBin(toread, what = "raw", size = 1, n = finfo$size)
              close(toread)
            } else if (is.VALID(input)) {
              get_URL <- httr::GET(input)
              raw_input <- get_URL$content
            }
            else {                                # String
              raw_input <- charToRaw(input)
            }
          },
          default = stop("Unknown input-type, please report the type of the input."
         )
  )
  return(raw_input)
}


