# R Utility functions for RbaseXClient.
# Works with BaseX 8.0 and later
#
# Documentation: http://docs.basex.org/wiki/Clients
#
# (C) Ben Engbers
#' Title RbaseX_Utility
#'
#' @return 'Raw' vector
#'
#' @param input Character vector length 1
#' @param addZero If TRUE, add a zero-byte (0x00) to the raw-vector
#'
#' @description Convert \emph{input} to a length-1 character vector.
#'
#' @details If \emph{input} is a reference to a file, the number of bytes
#'     corresponding to the size is read. If it is an URL, the URL is read and converted to a 'Raw' vector.
#'     The function does not catch errors.
#'
#' @export
input_to_raw <- function(input, addZero = FALSE) {
  type <- typeof(input)
  switch (type,
          "raw"       = raw_input <- input,             # Raw
          "character" = {
            if (input == "") {                          # Empty input
              raw_input <- raw(0)
            } else if (file.exists(input)) {            # File on filesystem
              finfo <- file.info(input)
              toread <- file(input, "rb")
              raw_input <- readBin(toread, what = "raw", size = 1, n = finfo$size)
              close(toread)
            }  else if (url.exists(input)) {            # URL
              get <- getURL(input)
              raw_input <- charToRaw(get)}
            else {                                      # String
              raw_input <- charToRaw(input)
            }
          },
          default = stop("Unknown input-type, please report the type of the input.")
  )
  if (addZero) raw_input <- c(raw_input, c(0x00)) %>% as.raw()

  return(raw_input)
}
