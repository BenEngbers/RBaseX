test_that("Query-object is created and executed", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "Test", password = "testBaseX")

  Session$set_intercept(FALSE)
  expect_error(Session$Query())
  Session$set_intercept(TRUE)
  Session$Query()                   # A string must be provided
  expect_equal(Session$get_success(), FALSE)

  Query_1 <- Session$Query(paste("for $i in 1 to 2", "return $i", sep = " "))
  expect_equal(Session$get_success(), TRUE)
  res <- Query_1$queryObject$ExecuteQuery()
  t1 <- c("1", "2")
  expect_equal(t1, res$Result)

  Query_2 <- Session$Query(paste("for $i in 3 to 4", "return i", sep = " "))
  expect_equal(Session$get_success(), TRUE)
  res <- Query_2$queryObject$ExecuteQuery()
  expect_equal(Session$get_success(), TRUE)
  # Cleanup
  Query_1$queryObject$Close()
  Query_2$queryObject$Close()
  expect_equal(Session$get_success(), TRUE)
  rm(Session)
})
