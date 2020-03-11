#' @title SocketClass
#'
#' @description Al methods that are used by BasexClient and QueryClass
#'
#' @export
SocketClass <- R6Class(
  "SocketClass",
  portable = TRUE,
  public = list(
    #' @description Initialize a new socket
    #' @param host,port,username,password Host-information and credentials
    initialize = function(host, port = 1984L, username, password) {
      private$CreateSocket(host, port, username, password)
    },
    #' @description When releasing the session-object, close the socketConnection
    finalize = function() {
      close(private$conn)
    },

    #' @description Return a boolean that indicates the result from the last action on the socket
    #' @param socket Socket-ID
    bool_test_sock = function(socket) {
      if (missing(socket)) socket <- self$get_socket()
      test <- readBin(socket, what = "raw", n =1)
      return(test == 0x00)
    },

    # All sent/received strings are utf8 strings or raw data, suffixed with a \00 byte.
    # To avoid confusion with this end-of-string byte, all transfered \00 and \FF bytes
    # are prefixed by an additional \FF byte.

    #' @description Send input to the socket
    #' @details Input is either a string or data that is read from a stream
    #' @param input Input
    void_send = function(input) {
      if (class(input) == "character") {
        streamOut <- charToRaw(input)
      } else {
        rd_id <- 1
        end <- length(input)
        streamOut <- raw()
        while (rd_id <= end) {
          rd <- c(input[rd_id])
          if (rd == 255 || rd == 0) streamOut <- c(streamOut, c(0xFF))
          rd_id <- rd_id + 1
          streamOut <- c(streamOut, rd)
        }
      }
      streamOut <- c(streamOut, c(0x00)) %>% as.raw()
      writeBin(streamOut, self$get_socket())
    },

    #' @description Read a string from a stream
    #' @details This method is not intented to be called direct
    #' @param input,output Input- and output-stream
    #' @param bin Boolean; TRUE when str_receive has to retrieve binary data
    #
    str_receive = function(input, output, bin = FALSE) {
      if (missing(input)) input   <- self$get_socket()
      if (missing(output)) output <- raw(0)
      while ((rd <- readBin(input, what = "raw", n =1)) > 0) {
        if (rd == 0xff) rd <- readBin(input, what = "raw", n =1)
        output <- c(output, rd)
      }
      # The 'Full'-method embeds a \0 in the output
      if (!bin) ret <- strip_CR_NUL(output) %>% rawToChar()
      else ret <- output
      return(ret)
      },

    #' @description Get socket-ID
    get_socket = function() {private$conn}
  ),

  private = list(
    conn = NULL,
    CreateSocket = function(host, port = 1984L, username, password) {
      tryCatch(
        {private$conn <- socketConnection(host = "localhost", port,
          open = "w+b", server = FALSE, blocking = TRUE, encoding = "utf-8", timeout = 1)
        },
        error = function(e) {
          stop("Cannot open the connection")}
      )
      response <- self$str_receive()
      splitted <-strsplit(response, "\\:")
      ifelse(length(splitted[[1]]) > 1,
        { code  <- paste(username, splitted[[1]][1],password, sep=":")
          nonce <- splitted[[1]][2]},
        { code  <- password
          nonce <- splitted[[1]][1]}
        )
      code <- md5(paste(md5(code), nonce, sep = ""))
      class(code) <- "character"
      self$void_send(username)
      self$void_send(code)
      if (!self$bool_test_sock()) {
        close(private$conn)
        stop("Access denied")
      }
    }
  )
)

strip_CR_NUL <- function(cache_in) {
  nul <- which(0  == cache_in); if (length(nul) > 0) cache_in[nul] <- charToRaw("|")
  CR  <- which(13 == cache_in); if (length(CR) > 0) cache_in <- cache_in[-CR]
  return(cache_in)
}
