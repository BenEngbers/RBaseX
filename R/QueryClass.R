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
    initialize = function(query, Parent) {
      private$parent <- Parent
      private$sock <- Parent$get_socket()
      exec <- c(as.raw(0x00), addVoid(query))
      response <- private$sock$handShake(exec) %>% split_Response()
      private$raw_id <- charToRaw(response[[1]]) %>% append(0) %>% as.raw()
      private$parent$set_success(response[[2]])
    },

    #' @description Executes a query.
    ExecuteQuery = function() {
      exec <- c(as.raw(0x05), private$raw_id)
      response <- private$sock$handShake(exec) %>% split_Response()

      names(response) <- c("Result", "success")
      response %<>% private$handle_response()
      return(response)
      invisible(self)
    },

    #' @description Binds a value to a variable.
    #' @details When using the primitive functions, this function can be chained.
    #' @param query_obj QueryClass instance-ID
    #' @param ... Binding Information
    Bind = function(...) {
      arguments <- list(...)
      name <-  arguments[[1]]; value <- arguments[[2]]
      argCnt <- length(arguments)
      if (argCnt == 2) {
        type <- ""
        if (!is.character(value)) {                        # single name/value tupple
          values <- raw(0)
          lapply(lapply(value, '[[',1), function(x) {values <<- c(values,charToRaw(x), c(0x01))})
          values <- head(values, -1)                       # Remove last 0x01
          value <- values %<>% as.raw()
        }
      } else {
        type <- arguments[[3]]
        if (!(is.character(value) && is.character(type))) {  # single name/value/type tupple
          values <- raw(0)                                   # bind name to sequence values and types
          mapply(function(val, typ)
          {values <<- c(values, charToRaw(val), c(0x02), charToRaw(typ),(0x01))},
          value, type)
          values <- head(values, -1)                       # Remove last 0x01
          value <- values %<>% as.raw()
          type <- ""
        }
      }

      exec <- c(as.raw(0x03), private$raw_id,
                addVoid(name), addVoid(value), addVoid(type))
      response <- private$sock$handShake(exec) %>% split_Response()

      names(response) <- c("Binding", "success")
      response %<>% private$handle_response()
      return(response)
      invisible(self)
    },

    #' @description Binds a value to the context. The type will be ignored if the string is empty.
    #' @details When using the primitive functions, this function can be chained.
    #' @param value Value that should be boud to the context
    #' @param type The type will be ignored when the string is empty
    Context = function(value, type) {
      if (missing(type)) type <- as.raw(0x00)

      exec <- c(as.raw(0x0E), private$raw_id,
                addVoid(value), addVoid(type))
      response <- private$sock$handShake(exec) %>% split_Response()

      names(response) <- c("queryObject", "success")
      response %<>% private$handle_response()
      return(response)
      invisible(self)
    },

    #' @description Executes a query and returns a vector with all resulting items as strings,
    #'     prefixed by the 'XDM' (Xpath Data Model) Meta Data <https://www.xdm.org/>.
    Full = function() {
      exec <- c(as.raw(0x1F), private$raw_id)
      response <- private$sock$handShake(exec)

      errors <- which(response == as.raw(c("01")))
      error <- (length(errors) > 0)
      private$parent$set_success(error)
      if (error) {
        response <- split_Response(response)
      } else {
        resp_list <- head(response, -3) %>% strip_CR() %>% strip_FF()
        if (length(response) == 2) {                       # Read was succesfull but had no results
          result <- list()
        } else {
          zero <- which(resp_list == 00)
          sta <- c(1, zero +1); sto <- c(zero, length(resp_list))
          result <-
            mapply(function(sta, sto, vec) {as.raw(vec[sta:sto])}, sta, sto, MoreArgs = list(resp_list) ) %>%
            lapply(function(x) {unlist(list(head(x, 1) %>% as.character(), x %>% rawToChar()))})
        }
        response <- list(result, !error)
      }
      names(response) <- c("fullResult", "success")
      return(response)
    },

    #' @description Indicates if there are any other results in the query-result.
    More = function() {
      if (is.null(private$cache)) {                        # The cache has to be filled
        exec <- c(as.raw(0x04), private$raw_id)
        response <- private$sock$handShake(exec)

        private$pos <- 0
        if (identical(response, as.raw(c(0,0)))) {
          private$cache <- list()
        } else {
          if (is.PLATFORM("Windows")) response %<>%  strip_CR()
          resp_list <- head(response, -3)

          result <- c()
          zero <- which(resp_list == 00)
          sta <- c(1, zero +1); sto <- c(zero -1, length(resp_list))
          result <- mapply(function(start, stop, vec) {vec[start:stop]}, sta, sto, MoreArgs = list(resp_list) , SIMPLIFY = FALSE)
          result %<>% lapply(function(x) {unlist(list(head(x, 1) %>% as.character(), tail(x, -1) %>% rawToChar()))})

          private$cache <- result
        }
      }
      if ( length(private$cache) > private$pos) return(TRUE)
      else {
        private$cache <- NULL
        private$pos <- 0
        return(FALSE)
      }},

    #' @description Returns the next result when iterating over a query
    Next = function() {
      if (self$More()) {
        private$pos <- private$pos + 1
        result <- private$cache[private$pos]
      }
      return(result)},

    #' @description Returns a string with query compilation and profiling info.
    Info = function() {
      exec <- c(as.raw(0x06), private$raw_id)
      response <- private$sock$handShake(exec) %>% split_Response()

      names(response) <- c("Info", "success")
      response %<>% private$handle_response()
      return(response)
    },

    #' @description Returns a string with all query serialization parameters, which
    #'     can e.g. be assigned to the serializer option.
    Options = function() {
      exec <- c(as.raw(0x07), private$raw_id)
      response <- private$sock$handShake(exec) %>% split_Response()

      names(response) <- c("Options", "success")
      response %<>% private$handle_response()
      if (identical(response$Options, character(0))) response$Options <- "No options set"
      return(response)
    },

    #' @description Check if the query contains updating expressions.
    Updating = function() {
      exec <- c(as.raw(0x1E), private$raw_id)
      response <- private$sock$handShake(exec) %>% split_Response()

      names(response) <- c("result", "success")
      response %<>% private$handle_response()
      return(response)
    },

    #' @description     Closes and unregisters the query with the specified ID
    #' @details When using the primitive functions, this function can be chained.
    Close = function() {
      exec <- c(as.raw(0x02), private$raw_id)
      response <- private$sock$handShake(exec) %>% split_Response()

      names(response) <- c("info", "success")
      response %<>% private$handle_response()
      return(response)
      invisible(self)
    }
  ),

  private = list(
    parent = NULL,
    sock = NULL,
    raw_id = NULL,
    cache = NULL,
    pos = NULL,
    # req_result = NULL,

    handle_response = function(Response) {
      private$parent$set_success(Response$success)
      Response[[1]]  %<>% strsplit("\n")
      Response[[1]] <- Response[[1]][[1]][which(Response[[1]][[1]] !="")]
      if (Response$success || (!Response$success && self$get_intercept()))
        return(Response)
      else {
        errorMsg <- Response[[1]]
        stop(errorMsg)
      }
    },
    clean = function(input) {
      if (input == "") return(input)
      else {
        result <- input %>% strsplit("\n", fixed = TRUE)
        if ((result[[1]][1]  == "")) result <- result[[1]][2]
      }
      return(result)
    }
  )
)
