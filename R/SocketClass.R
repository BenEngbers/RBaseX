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

    #' @description Send input to the socket and return the response
    #' @details Input is a raw vector, built up by converting all input to raw and concatenating the results
    #' @param input Input
    handShake = function(input) {
      writeBin(input, private$conn)
      return(readBin_(private$conn))
    },

    #' @description Write 1 byte to the socket
    #' @param Byte A  vector length 1
    write_Byte = function(Byte) {
      writeBin(Byte, private$conn)
      invisible(self)
    }
  ),

  private = list(
    conn = NULL,
    sendInput = function(input) {
      writeBin(input, private$conn)
      invisible(self)
    },
    CreateSocket = function(host, port = 1984L, username, password) {
      tryCatch(
        {conn <- private$conn <- socketConnection(
          host = "localhost", port,
          open = "w+b", server = FALSE, blocking = TRUE, encoding = "UTF-8", timeout = 1)
        }, error = function(e) {
          stop("Cannot open the connection")
        }
      )
      response <- readBin_(conn) %>% rawToChar()
      splitted <-strsplit(response, "\\:")
      ifelse(length(splitted[[1]]) > 1,
             { realm <- splitted[[1]][1]
               code  <- paste(username, realm, password, sep=":")
               nonce <- splitted[[1]][2] },
             { code  <- password
               nonce <- splitted[[1]][1]}
            )
      code <- md5(paste(md5(code), nonce, sep = "")) %>% charToRaw()
      # send username + code
      auth <- c(charToRaw(username), as.raw(0x00), code, as.raw(0x00))
      writeBin(auth, private$conn)
      Accepted <- readBin(conn, what = "raw", n = 1) == 0x00
      if (!Accepted) {
        close(private$conn)
        stop("Access denied")
      }
    }
  )
)

readBin_ <- function(conn) {
  chars_read <- raw(0)
  rd <- readBin(conn, what = "raw", 1024)
  while(length(rd) == 1024) {
    chars_read <- c(chars_read, rd)
    rd <- readBin(conn, "raw", 1024)
    }
  if (length(rd) > 0) chars_read <- c(chars_read, rd)
  return(chars_read)
}
