#' @title SocketClass
#'
#' @description All methods that are used by BasexClient and QueryClass
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
          open = "w+b", server = FALSE, blocking = FALSE, encoding = "UTF-8")
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
      socketSelect(list(conn))
      Accepted <- readBin(conn, what = "raw", n = 1) == 0
      if (!Accepted) {
        close(private$conn)
        stop("Access denied")
      }
    }
  )
)

readBin_ <- function(conn) {
  total_read <- rd <- as.raw(c())
  while(!done(rd, length(total_read))) {
    socketSelect(list(conn))
    rd <- readBin(conn, "raw", 1024)
    total_read <- c(total_read,rd)
    }
  return(total_read)
}
done <- function(rd, total_length) {
  finish <- TRUE
  if (total_length == 0) {
    finish <- FALSE
  } else {
    i <- length(rd)
    if (i ==1024) finish <- FALSE
  }
  return(finish)
}
