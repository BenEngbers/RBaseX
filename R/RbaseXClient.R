# R client for 'BaseX'.
# Works with BaseX 8.0 and later

# (C) Ben Engbers

#' @title BasexClient
#' @docType package
#' @name RBaseX
#'
#' @description The client can be used in 'standard' mode and in 'query' mode.
#'     Standard Mode is used for connecting to a server and sending commands.
#'
#' @export
BasexClient <- R6Class(
  "BasexClient",
  portable = TRUE,
  public = list(
    #' @description Initialize a new client-session
    #' @param host,port,username,password Host-information and user-credentials
    initialize = function(host, port = 1984L, username, password) {
      private$sock <- SocketClass$new(host, port = 1984L, username, password)
    },

    #' @description Execute a command
    #' @param command Command
    #' @details For a list of database commands see \url{http://docs.basex.org/wiki/Commands}
    Execute = function(command) {
      bin <- if (grepl("retrieve\\s+", command, ignore.case = TRUE)) TRUE
      else FALSE
      private$sock$void_send(command)
      # The server responds by sending {result} {info} 0x00 and a status-byte
      # Status 0x00 means success, 0x01 means error
      result <- private$sock$str_receive(bin = bin)
      info <-   private$sock$str_receive()
      if (class(result) == "character") {result <- result %>% strsplit("\n")}
      if (length(info) > 0) cat(info, "\n")

      success = private$sock$bool_test_sock()
      self$set_success(success)

      if (success || (!success && self$get_intercept()))
        return(list(result = result, info = info, success = self$get_success()))
      else stop(info)
    },

    #' @description Create a new query-object
    #' @details A query-object has two fields. 'queryObject' is an ID for the new created 'QueryClass'-instance.
    #'     'success' holds the status from the last executed operation on the queryObject.
    #' @param query Query-string
    #' @return ID for the created query-object
    Query = function(query) {
      if (missing(query)) {
        self$set_success(FALSE)
        if (self$get_intercept()) {
          return(list(queryObject = NULL, success = self$get_success()))
        } else stop("No query-string provided")
      }
      tryCatch(
        { queryObject <- QueryClass$new(query, self)
          success <- private$sock$bool_test_sock()
          self$set_success(success)
          return(list(queryObject = queryObject, success = self$get_success()))
        },
        error = function(e) {
          success <- private$sock$bool_test_sock()
          self$set_success(success)
          if (self$get_intercept()) {
            return(list(queryObject = NULL, success = self$get_success()))
          } else {
            message("Error creating the query-object")
            stop()}
          }
        )
    },

    #' @description Add a new resouce at the specified path
    #' @param path Path
    #' @param input File, directory or XML-string
    Add = function(path, input) {
      private$default_pattern(match.call()[[1]], path, input)
      invisible(self)
    },

    #' @description Create a new database
    #' @details Initial content can be offered as string, URL or file.
    #' @param name Name
    #' @param input Initial content, Optional
    Create = function(name, input) {
      if (missing(input)) input <- ""
      private$default_pattern(match.call()[[1]], name, input)
      invisible(self)
    },

    #' @description Replace resource, adressed by path
    #' @param path Path
    #' @param input File, directory or XML-string
    Replace = function(path, input) {
      private$default_pattern(match.call()[[1]], path, input)
      invisible(self)
    },

    #' @description Store binary content
    #' @details Binary content can be retrieved by executing a retrieve-command
    #' @param path Path
    #' @param input File, directory or XML-string
    Store = function(path, input) {
      private$default_pattern(match.call()[[1]], path, input)
      invisible(self)
    },

    #' @description Toggles between using the ´success'-field, returned by the
    #'     Execute-command or using regular error-handling (try-catch).
    #' @details sgfdsffdsh
    #' @param Intercept Boolean
    set_intercept = function(Intercept) {
      private$Intercept_Old = private$Intercept
      private$Intercept = Intercept
      invisible(self)
    },
    #' @description Restore the Intercept Toggles to the original value
    restore_intercept = function() {
      private$Intercept = private$Intercept_Old
      invisible(self)
    },
    #' @description Get current Intercept
    get_intercept = function() {
      private$Intercept
    },
    #' @description Get the socket-ID
    #' @return Socket-ID,
    get_socket = function() {
      private$sock},
    #' @description Set the status success-from the last operation on the socket
    #' @details This function is intended to be used by instances from the QueryClass
    #' @param Success Boolean
    set_success = function(Success) {
      private$Success <- Success},
    #' @description Get the status success-from the last operation on the socket
    #' @return Boolean,
    get_success = function() {
      private$Success}
  ),

  private = list(
    sock = NULL,
    Success = NULL,
    Intercept = FALSE,
    Intercept_Old = NULL,
    default_pattern = function(Caller, path, input) {
      if (missing(path) || missing(input)) {
        stop("'path' and/or 'input' are missing")
      } else {
        switch(as.character(Caller[[3]]),
               "Create"  =  writeBin(as.raw(0x08), private$sock$get_socket()),
               "Add"     =  writeBin(as.raw(0x09), private$sock$get_socket()),
               "Replace" =  writeBin(as.raw(0x0C), private$sock$get_socket()),
               "Store"   =  writeBin(as.raw(0x0D), private$sock$get_socket())
        )
        private$sock$void_send(path)
        # browser()
        input <- input_to_raw(input)
        private$sock$void_send(input)
        # The server responds by sending {info}, 0x00 and a status-byte
        # Status 0x00 means success, 0x01 means error
        partial <- private$sock$str_receive()

        success = private$sock$bool_test_sock()
        self$set_success(success)

        if (success || (!success && self$get_intercept()))
          return(list(info = partial, success = self$get_success))
        else {
          errorMsg <- private$sock$str_receive()
          close(private$sock$get_socket())
          stop(errorMsg)
        }
      }
    }
  )
)
