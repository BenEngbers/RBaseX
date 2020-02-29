#' @title QueryClass
#'
#' @description The client can be used in 'standard' mode and in 'query' mode.
#'     Query mode is used to define queries, binding variables and for iterative evaluation.
#'
#' @export
QueryClass <- R6Class(
  "QueryClass",
  portable = TRUE,
  public = list(
    #' @description Initialize a new instance from QueryClass
    #' @details QueryClass-instances can only be created by calling the 'Query'-method from
    #'     the 'BasexClient'-class.
    #' @param query Query-string
    #' @param Parent The 'Parent' for this QueryClass-instance
    #' @param sock Session-socket
    #' @param Intercept Pointer to the Intercept-method from the Session-object
    initialize = function(query, Parent) {
      private$parent <- Parent
      private$sock <- Parent$get_socket()
      out_stream <- private$sock$get_socket()
      writeBin(as.raw(0x00), out_stream)
      private$sock$void_send(query)
      private$raw_id <- charToRaw(private$sock$str_receive()) %>% append(0) %>% as.raw()
    },

    #' @description Binds a value to a variable.
    #' @details When using the primitive functions, this function can be chained.
    #' @param query_obj QueryClass instance-ID
    #' @param ... Binding Information
    Bind = function(...) {
      socket <- private$sock$get_socket()
      private$write_code_ID(0x03)
      writeBin(Binding(list(...)), socket)
      private$req_result <- private$sock$str_receive()

      success <- private$parent$get_socket()$bool_test_sock()
      private$parent$set_success(success)
      if (success || (!success && private$parent$get_intercept()))
        return(list(queryObject = NULL, success = private$parent$get_success()))
      else stop(private$sock$str_receive())
      invisible(self)
    },

    #' @description Binds a value to the context. The type will be ignored if the string is empty.
    #' @details When using the primitive functions, this function can be chained.
    #' @param value Value that should be boud to the context
    #' @param type The type will be ignored when the string is empty
    Context = function(value, type) {
      if (missing(type)) type <- ""
      socket <- private$sock$get_socket()
      private$write_code_ID(0x0E)
      cont_info <- charToRaw(value) %>% append(0) %>% as.raw()
      if (type == "") {
        cont_info %<>% append(0x00) %>% as.raw()
      } else {
        cont_info %<>% append(charToRaw(type)) %>% append(0x00) %>% as.raw()
      }
      writeBin(cont_info, socket)
      private$req_result <- private$sock$str_receive()

      success <- private$parent$get_socket()$bool_test_sock()
      private$parent$set_success(success)
      if (success || (!success && private$parent$get_intercept()))
        return(list(queryObject = NULL, success = private$parent$get_success()))
      else stop(private$sock$str_receive())
      invisible(self)
    },

    #' @description     Closes and unregisters the query with the specified ID
    #' @details When using the primitive functions, this function can be chained.
    Close = function() {
      private$default_query_pattern(match.call()[[1]])
      invisible(self)
    },

    #' @description Executes a query.
    ExecuteQuery = function() {
      private$cache <- NULL
      private$default_query_pattern(match.call()[[1]])
    },

    #' @description Returns a string with query compilation and profiling info.
    Info = function() {
      private$default_query_pattern(match.call()[[1]])
    },

    #' @description Returns a string with all query serialization parameters, which
    #'     can e.g. be assigned to the serializer option.
    Options = function() {
      private$default_query_pattern(match.call()[[1]])
      return(ifelse(!private$req_result == "",
                    private$req_result %>% private$clean(), "No options set")
      )
    },

    #' @description Check if the query contains updating expressions.
    Updating = function() {
      private$default_query_pattern(match.call()[[1]])
      private$req_result %<>% as.logical()
    },

    #' @description Indicates if there are any other results in the query-result.
    More = function() {
      if (is.null(private$cache)) { # The cache has to be filled
        in_stream <- private$sock$get_socket()
        private$write_code_ID(0x04)
        cache <- c()
        while ((rd <- readBin(in_stream, what = "raw", n =1)) > 0) {
          cache <- c(cache, as.character(rd))
          cache <- c(cache, private$sock$str_receive())
        }
        success <- private$parent$get_socket()$bool_test_sock()
        private$parent$set_success(success)
        private$cache <- cache
        private$pos <- 0
      }
      if ( length(private$cache) > private$pos) return(TRUE)
      else {
        private$cache <- NULL
        return(FALSE)
      }},

    #' @description Returns the next result when iterating over a query
    Next = function() {
      if (self$More()) {
        private$pos <- private$pos + 1
        result <- private$cache[private$pos]
      }
      return(result)},

    #' @description Executes a query and returns a vector with all resulting items as strings,
    #'     prefixed by the 'XDM' (Xpath Data Model) Meta Data <https://www.xdm.org/>.
    Full = function() {
      in_stream <- out_stream <- private$sock$get_socket()
      private$write_code_ID(0x1F)
      cache <- c()
      while ((rd <- readBin(in_stream, what = "raw", n =1)) > 0) {
        cache <- c(cache, as.character(rd))
        add_cache <- private$sock$str_receive()
        cache <- c(cache, add_cache)
      }
      private$parent$get_socket()$bool_test_sock()
      result <- cache
      return(result)
    }
  ),

  private = list(
    parent = NULL,
    sock = NULL,
    raw_id = NULL,
    cache = NULL,
    pos = NULL,
    req_result = NULL,
    write_code_ID = function(id_code) {
      out_stream <- private$sock$get_socket()
      writeBin(as.raw(id_code), out_stream)
      writeBin(private$raw_id, out_stream)},
    receive_more = function(input, output) {
      if (missing(input)) input   <- private$get_sock()
      if (missing(output)) output <- raw(0)
      while ((rd <- readBin(input, what = "raw", n =1)) > 0) {
        if (rd == 0xff) next
        output <- c(output, rd)
      }
      ret <- rawToChar(output)
      return(ret)},
    clean = function(input) {
      if (input == "") return(input)
      else {
        result <- input %>% strsplit("\n", fixed = TRUE)
        if ((result[[1]][1]  == "")) result <- result[[1]][2]
      }
      return(result)
    },
    default_query_pattern = function(Caller) {
      switch(as.character(Caller[[3]]),
             "Close"        =  private$write_code_ID(0x02),
             "ExecuteQuery" =  private$write_code_ID(0x05),
             "Info"         =  private$write_code_ID(0x06),
             "Options"      =  private$write_code_ID(0x07),
             "Updating"     =  private$write_code_ID(0x1E)
      )
      private$req_result <- private$sock$str_receive()
      success <- private$parent$get_socket()$bool_test_sock()
      private$parent$set_success(success)
      if (success || (!success && private$parent$get_intercept()))
        result <- private$req_result %>% private$clean()
      else {
        error_msg <- private$sock$str_receive()
        self$Close()
        stop(error_msg)
      }
    }
  )
)

Binding <- function(...) {
  arguments <- list(...)[[1]]
  name <- arguments[[1]]
  vals <- arguments[[2]]
  argCnt <- length(arguments)

  retour <- charToRaw(name) %>% append(0) %>% as.raw()
  if (argCnt == 3) {                                        # Name, value and type
    retour %<>% append(charToRaw(vals)) %>% append(0x00) %>%
      append(charToRaw(arguments[[3]])) %>% as.raw()
  } else if (is.vector(vals)){                                # Name, list of values
    valLength <- length(vals)
    for (i in 1:length(vals)) {
      retour %<>% append(charToRaw(vals[[i]][[1]]))
      if (length(vals[[i]]) ==2) {
        retour %<>% append(0x02) %<>% as.raw() %<>%
          append(charToRaw(vals[[i]][[2]])) %<>% as.raw()
      }
      if (i < valLength) retour %<>% append(0x01) %<>% as.raw() else
        retour %<>% append(0x00) %<>% as.raw()
    }
  } else {                                                  # Name, value
    retour %<>% append(charToRaw(vals)) %>% append(0x00) %>%
      append(charToRaw("")) %>% as.raw()
  }
  retour %<>% append(0x00) %>% as.raw()
  return(retour)
}
