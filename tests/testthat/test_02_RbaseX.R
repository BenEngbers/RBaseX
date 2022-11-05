test_that("Credentials are checked", {
  skip_unless_socket_available()

  expect_error(BasexClient$new("localhost", 1984L, username = "admin", password = "denied"), "Access denied")
  Session <- BasexClient$new("localhost", 1984L, username = "Test", password = "testBaseX")
  expect_named(Session)
  # Cleanup
  rm(Session)
})

test_that("getSuccess functions", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "Test", password = "testBaseX")

  succ <- Session$get_success()
  expect_null(succ)
  Session$Command("info")
  expect_equal(Session$get_success(), TRUE)
  # Cleanup
  rm(Session)
})
