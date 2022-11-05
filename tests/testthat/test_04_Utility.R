test_that("Sequence result is converted to frame", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "Test", password = "testBaseX")

  Query_1 <- Session$Query(paste("declare variable $name external;",
                                  "for $i in 1 to 3 return ( element { $name } { $i },  $i, $i mod 2 = 0)"))
  Query_1$queryObject$Bind("$name", "number")
  re <- Query_1$queryObject$ExecuteQuery()

  res_f <- result2frame(re, 3)
  expect_equal(nrow(res_f), 3)
  expect_equal(class(res_f), "data.frame")
  expect_equal(lapply(res_f, class)[[3]], "logical")

  # Cleanup
  Query_1$queryObject$Close()
  rm(Session)
})

test_that("Array result is converted to frame", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "Test", password = "testBaseX")

  Query_1 <- Session$Query(paste("for $i in 1 to 2 return ( [$i, math:pow($i, 2), string(math:pow($i, 3)), $i mod 2 = 0])"))
  re_arr <- Query_1$queryObject$ExecuteQuery()

  res_f <- result2frame(re_arr)
  expect_equal(class(res_f), "data.frame")
  expect_equal(lapply(res_f, class)[[4]], "logical")

  # Cleanup
  Query_1$queryObject$Close()
  rm(Session)
})

test_that("Sequence result is converted to tibble", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "Test", password = "testBaseX")

  Query_1 <- Session$Query(paste("declare variable $name external;",
                                  "for $i in 1 to 3 return ( element { $name } { $i },  $i, $i mod 2 = 0)"))
  Query_1$queryObject$Bind("$name", "number")
  re <- Query_1$queryObject$ExecuteQuery()

  res_t <- result2tibble(re, 3)
  expect_equal(nrow(res_t), 3)
  expect_equal(class(res_t)[[1]], "tbl_df")
  expect_equal(lapply(res_t, class)[[3]], "logical")

  # Cleanup
  Query_1$queryObject$Close()
  rm(Session)
})

test_that("Array result is converted to tibble", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "Test", password = "testBaseX")

  Query_1 <- Session$Query(paste("for $i in 1 to 2 return ( [$i, math:pow($i, 2), string(math:pow($i, 3)), $i mod 2 = 0])"))
  re_arr <- Query_1$queryObject$ExecuteQuery()

  res_t <- result2tibble(re_arr)
  expect_equal(class(res_t)[[1]], "tbl_df")
  expect_equal(lapply(res_t, class)[[4]], "logical")

  # Cleanup
  Query_1$queryObject$Close()
  rm(Session)
})
