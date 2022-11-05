library(RBaseX)
library(testthat)
library(glue)

skip_unless_socket_available <- function() {
    tryCatch({
    sock <- base::socketConnection(
      host = "localhost", 1984,
      open = "w+b", server = FALSE, blocking = FALSE, encoding = "utf-8")
    close(sock)
    TRUE
  }, error = function(e) {
    skip(paste0("basexserver not available:\n'", conditionMessage(e), "'"))
  })
}
