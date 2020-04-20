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
    bool_test_sock = function() {
      socket <- self$get_socket()
      test <- readBin(socket, what = "raw", n = 1)
      if (test == 0x00) response <- TRUE
      else if (test == 0x01) response <- FALSE
      else stop("SocketTest reports unknown failure")
      return(response)
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
        streamOut <- prep_bin_out(input)
      }
      streamOut <- c(streamOut, c(0x00)) %>% as.raw()  # Append \0x00 -byte
      writeBin(streamOut, self$get_socket())
    },

    #' @description Read a string from a stream
    #' @param bin Logical; TRUE when str_receive has to retrieve binary data
    #
    str_receive = function(bin = FALSE) {
      socket_in   <- self$get_socket()
      if (!bin) {                                 # Character
        string_read <- socket_char_reader(socket_in)
      } else {                                    # binary
        string_read <- socket_bin_reader(socket_in)
      }
      return(string_read)
    },

    #' @description Write 1 byte to the socket
    #' @param Byte A  vector length 1
    write_Byte = function(Byte) {
      writeBin(Byte, self$get_socket())
    },

    #' @description Read 1 byte to the socket
    read_Byte = function() {
      socket <- self$get_socket()
      return(readBin(socket, what = "raw", n=1))
    },

    #' @description Get socket-ID
    get_socket = function() {private$conn}
  ),

  private = list(
    conn = NULL,
    CreateSocket = function(host, port = 1984L, username, password) {
      tryCatch(
        {private$conn <- socketConnection(
          host = "localhost", port,
          open = "w+b", server = FALSE, blocking = TRUE, encoding = "UTF-8", timeout = 1)
        }, error = function(e) {
          stop("Cannot open the connection")
        }
      )
      # browser()
      response <- self$str_receive()
      splitted <-strsplit(response, "\\:")
      ifelse(length(splitted[[1]]) > 1,
             { realm <- splitted[[1]][1]
               code  <- paste(username, realm, password, sep=":")
               nonce <- splitted[[1]][2] },
             { code  <- password
               nonce <- splitted[[1]][1]}
            )
      code <- md5(paste(md5(code), nonce, sep = ""))
      class(code) <- "character"
      self$void_send(username)
      self$void_send(code)
      Accepted <- self$bool_test_sock()
      if (!Accepted) {
        close(private$conn)
        stop("Access denied")
      }
    }
  )
)

socket_bin_reader <- function(in_sock) {
  string_read <- raw(0)
  while((rd <- readBin(in_sock, what = "raw", n=1)) > 0) {
    if (rd == 0xff) rd <- readBin(in_sock, what = "raw", n =1)
    string_read <- c(string_read, rd)
  }
  return(string_read)
}
socket_char_reader <- function(in_sock) {
  string_read <- raw(0)
  while((rd <- readBin(in_sock, what = "raw", n=1)) > 0) {
    string_read <- c(string_read, rd)
  }
  return(string_read %>% strip_CR_NUL() %>% rawToChar())
}
prep_bin_out <- function(bin_input) {
  streamOut <- bin_input
  end <- length(bin_input)
  if (end > 0) {
    streamOut <- add_FF(streamOut)
    return(streamOut)}
  else {
    return(bin_input)
  }
  return(streamOut)
}

strip_CR_NUL <- function(cache_in) {
  nul <- which(0  == cache_in); if (length(nul) > 0) cache_in[nul] <- charToRaw("|")
  CR  <- which(13 == cache_in); if (length(CR) > 0) cache_in <- cache_in[-CR]
  return(cache_in)                                # is raw()
}
strip_FF <- function(cache_in) {
  FF <- which(255  == cache_in)
  stripped <- cache_in
  if (length(FF) > 0) {
    remove <- c()
    cache_length <- length(cache_in)
    for (i in 1:length(FF)) {
      if (FF[i] < cache_length &&(cache_in[FF[i]+1] == 255 || cache_in[FF[i]+1] == 0)) {
        remove <- c(remove, FF[i])
        i <- i+1
      }
    }
    if (length(remove) > 0) stripped <- cache_in[-remove] %>% as.raw()
  }
  return(stripped)                                # is raw()
}
add_FF <- function(cache_in) {
  FF <- which(255  == cache_in)
  Z  <- which(0  == cache_in)
  if ((length(FF) > 0) || (length(Z) > 0)) {
    add_FF <- c(FF, Z)
    FFed <- c()
    for (i in 1:length(cache_in)) FFed[2*i] <- cache_in[i]
    for (i in 1:length(add_FF)) {
      FFed[2*add_FF[i] -1] <- as.raw(255)
    }
    remove <- which(0 == FFed)
    FFed <- FFed[-remove] %>% as.raw()
    return(FFed)
  } else
    return(cache_in)
}
