test_that("Result is converted to frame", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  Query_1 <- Session$Query(paste0("declare variable $name external;",
                                  "for $i in 1 to 3 return ( element { $name } { $i },  $i)"))
  Query_1$queryObject$Bind("$name", "number")
  re <- Query_1$queryObject$ExecuteQuery()

  res_m <- result2matrix(re, 2)
  res_f <- result2frame(re, 2)
  res_t <- result2tibble(re, 2)
  expect_equal(nrow(res_f), 3)

  # Cleanup
  Query_1$queryObject$Close()
  rm(Session)
})
