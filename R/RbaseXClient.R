# R client for 'BaseX'.
# Works with BaseX 8.0 and later

# ------------
# 20220131 Added invisible(self) to Execute
# ------------

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
    #' @details For a list of database commands see \url{https://docs.basex.org/wiki/Commands}
    Command = function(command) {
      exec <- c(raw(), addVoid(command))
      response <- private$sock$handShake(exec) %>% split_Response()

      # if (class(response[[1]]) == "character") response[[1]] %<>% strsplit("\n")
      if (inherits(response[[1]], "character")) response[[1]] %<>% strsplit("\n")
      response[[2]] %<>% clean_Response()
      names(response) <- c("result", "info", "success")
      return(private$handle_response(response))

      invisible(self)
    },

    #' @description Execute a command
    #' @param command Command
    #' @details For a list of database commands see \url{https://docs.basex.org/wiki/Commands}.
    #'    This function is replaced by 'Command' and is obsolete.
    Execute = function(command) {
      return(self$Command(command))
      invisible(self)
    },

    #' @description Create a new query-object
    #' @details A query-object has two fields. 'queryObject' is an ID for the new created 'QueryClass'-instance.
    #'     'success' holds the status from the last executed operation on the queryObject.
    #' @param query_string Query-string
    #' @return ID for the created query-object
    Query = function(query_string) {
      if (missing(query_string) || identical(query_string, "")) {
        self$set_success(FALSE)
        if (self$get_intercept()) {
          return(list(queryObject = NULL, success = self$get_success()))
        } else stop("No query-string provided")
      }
      tryCatch(
        { queryObject <- QueryClass$new(query_string, self)
          return(list(queryObject = queryObject, success = self$get_success()))
        },
        error = function(e) {
          if (self$get_intercept()) {
            return(list(queryObject = NULL, success = self$get_success()))
          } else {
            message("Error creating the query-object")
            stop()}
          }
        )
    },

    #' @description Create a new database
    #' @details Initial content can be offered as string, URL or file.
    #' @param name Name
    #' @param input Initial content, Optional
    Create = function(name, input) {
      if (missing(input)) input <- ""
      exec <- c(as.raw(0x08), addVoid(name), addVoid(input_to_raw(input)))
      response <- private$sock$handShake(exec) %>% split_Response()

      response[[1]] %<>% strsplit("\n")
      response[[1]][[1]] %<>% clean_Response()
      names(response) <- c("info", "success")

      return(private$handle_response(response))
      invisible(self)
    },

    #' @description Add a new resouce at the specified path
    #' @param path Path
    #' @param input File, directory or XML-string
    Add = function(path, input) {
      if (missing(path) || missing(input)) { stop("'path' and/or 'input' are missing")}

      exec <- c(as.raw(0x09), addVoid(path), addVoid(input_to_raw(input)))
      response <- private$sock$handShake(exec) %>% split_Response()

      response[[1]] %<>% strsplit("\n")
      response[[1]][[1]] %<>% clean_Response()
      names(response) <- c("info", "success")

      return(private$handle_response(response))
      invisible(self)
    },

    #' @description Replace resource, adressed by path
    #' @param path Path
    #' @param input File, directory or XML-string
    Replace = function(path, input) {
      exec <- c(as.raw(0x0C), addVoid(path), addVoid(input_to_raw(input)))
      response <- private$sock$handShake(exec) %>% split_Response()

      response[[1]] %<>% strsplit("\n")
      response[[1]][[1]] %<>% clean_Response()

      names(response) <- c("info", "success")
      return(private$handle_response(response))
      invisible(self)
    },

    #' @description Store binary content
    #' @details Binary content can be retrieved by executing a retrieve-command
    #' @param path Path
    #' @param input File, directory or XML-string
    Store = function(path, input) {
      input %<>% add_FF()

      exec <- c(as.raw(0x0D), addVoid(path), addVoid(input_to_raw(input)))
      response <- private$sock$handShake(exec) %>% split_Response()

      response[[1]] %<>% strsplit("\n")
      response[[1]][[1]] %<>% clean_Response()

      names(response) <- c("info", "success")
      return(private$handle_response(response))
      invisible(self)
    },

    #' @description Toggles between using the ´success'-field, returned by the
    #'     Execute-command or using regular error-handling (try-catch).
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

    handle_response = function(Response) {
      self$set_success(Response$success)
      if (Response$success || (!Response$success && self$get_intercept()))
        return(Response)
      else {
        errorMsg <- Response[[1]]
        stop(errorMsg)
      }
    }
  )
)
