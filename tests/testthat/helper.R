library(RBaseX)
library(testthat)

skip_unless_socket_available <- function() {
    tryCatch({
    sock <- base::socketConnection(
      host = "localhost", 1984,
      open = "w+b", server = FALSE, blocking = TRUE, encoding = "utf-8", timeout = 1)
    close(sock)
    TRUE
  }, error = function(e) {
    skip(paste0("basexserver not available:\n'", conditionMessage(e), "'"))
  })
}
