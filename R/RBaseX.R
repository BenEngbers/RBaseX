#' @title RBaseX
#' @docType package
#' @name RBaseX
#' @description 'BaseX' is a robust, high-performance XML database engine and a highly compliant XQuery 3.1 processor
#'     with full support of the W3C Update and Full Text extensions.
#'
#' @importFrom magrittr %>% %<>%
#' @import dplyr
#' @import utils
#' @import R6
#' @import RCurl
#' @import stringr
#' @import tibble
#' @importFrom data.table rbindlist
#' @importFrom openssl md5
#'
#' @details 'RBaseX' was developed using R6. For most of the public methods in the R6-classes, wrapper-functions
#'     are created. The differences in performance between R6-methods and wrapper-functions are minimal and
#'     slightly in advantage of the R6-version.
#'
#'     It is easy to use the R6-calls instead of the wrapper-functions.
#'     The only important difference is that in order to execute a query, you have to call ExecuteQuery()
#'     on a queryObject.
#'
#' @examples
#' \dontrun{
#'     Session <- BasexClient$new("localhost", 1984L, username = "<username>", password = "<password>")
#'     Session$Execute("Check test")
#'     Session$Execute("delete /")
#'     # Add resource
#'     Session$Add("test.xml", "<root/>")
#'
#'     # Bindings -----
#'     query_txt <- "declare variable $name external; for $i in 1 to 3 return element { $name } { $i }"
#'     query_obj <- Session$Query(query_txt)
#'     query_obj$queryObject$Bind("$name", "number")
#'     print(query_obj$queryObject$ExecuteQuery())
#' }

globalVariables(c("."))
"_PACKAGE"
