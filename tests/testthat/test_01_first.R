test_that("Credentials are accepted and a db is created", {
  skip_unless_socket_available()

  expect_error(SocketClass$new(host, port = 1984L, "admin", "denied"))
  expect_type(SocketClass$new("localhost", port = 1984L, "admin", "admin"), "environment")

  # Create and populate a Test-database
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  Session$set_intercept(TRUE)
  Session$Execute("drop DB TestDB")
  Session$Execute("Open TestDB")
  Opened <- Session$get_success()
  if (!Opened) {
    Session$Create("TestDB")
    Session$Add("Test.xml", "<Line_1 line='1'>Content 1</Line_1>")
    Session$Add("Test.xml", "<Line_2 line='2'>Content 2</Line_2>")
    Session$Add("Test.xml", "<Line_3 line='3'>Content 3</Line_3>")
    Session$Add("Books", "<book title='XQuery' author='Walmsley'/>")
    Add_Book <- "let $book := <book title='Advanced R' author='Wickham'/>
      return insert node $book as last into collection('TestDB/Books')"
    Query_obj <- Session$Query(Add_Book)
    Query_obj$queryObject$ExecuteQuery()
  }
  Session$Execute("Close")
  Session$restore_intercept()             # should be FALSE
  expect_equal(Session$get_intercept(), FALSE)
  # Cleanup
  rm(Session)
})
