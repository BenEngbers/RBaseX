test_that("Session, Database and QueryObjects are created and can be used", {
  skip_unless_socket_available()
  Session <- NewBasexClient(user = "admin", password = "admin")

  SetIntercept(Session, FALSE)
  expect_named(Execute(Session, "Info"))
  expect_error(Query(Session))
  SetIntercept(Session, TRUE)
  expect_true(GetIntercept(Session))
  Execute(Session, "Open TestOpen")        # Ensure that TestOpen does not exist
  if (GetSuccess(Session)) {
    Execute(Session,"Close")
    Execute(Session,"DROP DB TestOpen")
  }
  SetIntercept(Session, FALSE)             # should be FALSE
  expect_error(Execute(Session, "Open TestOpen"))
  SetIntercept(Session, TRUE)
  Execute(Session, "Open TestOpen")
  if (!GetSuccess(Session)) {
    Create(Session, "TestOpen")
  }
  Execute(Session, "Open TestOpen")
  expect_true(GetSuccess(Session))
  Execute(Session, "Close")
  Session$restore_intercept()             # should be FALSE

  RestoreIntercept(Session)
  expect_false(GetIntercept(Session))
  SetIntercept(Session, TRUE)
  Query_1 <- Query(Session)
  expect_equal(GetSuccess(Session), FALSE)
  Query_2 <- Query(Session, "for $i in 1 to 2 return i")
  expect_equal(GetSuccess(Session), TRUE)
  Query_3 <- Query(Session, "for $i in 1 to 2 return $i")
  expect_equal(GetSuccess(Session), TRUE)

  Execute(Session,"DROP DB TestOpen")

  # Cleanup
  Close(Query_2)
  Close(Query_3)
  rm(Session)
})

test_that("'Full' and 'More' output should differ", {
  skip_unless_socket_available()
  Session <- NewBasexClient(user = "admin", password = "admin")

  Query_1 <- Query(Session, "collection('TestDB/Test.xml')")
  fullResult <- Full(Query_1)
  expect_equal(substr(fullResult[[2]], 1, 16), "/TestDB/Test.xml")
  iterResult <- c()
  while (More(Query_1)) {
    iterResult <- c(iterResult, Next(Query_1))
  }
  expect_length(iterResult, 6)
  # Cleanup
  Close(Query_1)
  rm(Session)
})

test_that("Query is executed and Updating() is false", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  Query_1 <- Query(Session, "for $i in 1 to 2 return <xml>Text { $i }</xml>")
  expect_equal(Session$get_success(), TRUE)
  Query_2 <- Query(Session, "for $i in 3 to 4 return $i")
  expect_equal(Session$get_success(), TRUE)
  res <- Execute(Query_1)
  expect_length(res[[1]], 2)
  res <- Execute(Query_2)
  t2 <- list(c("3", "4"))
  expect_equal(t2, res)

  expect_false(Updating(Query_1))
  expect_output(str(Options(Query_1)), "No options set")
  # Cleanup
  Close(Query_1)
  Close(Query_2)
  rm(Session)
})

test_that("Binding and Context function", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  Q_txt <- "declare variable $name external; for $i in 1 to 3 return element { $name } { $i }"
  Query_1 <- Query(Session, Q_txt)
  Bind(Query_1, "$name", "number")
  expect_output(str(Execute(Query_1)), '"<number>1</number>" "<number>2</number>" "<number>3</number>"')

  ctxt_query <- Query(Session, "for $t in .//text() return string-length($t)")
  ctxt_txt   <- paste0("<xml>", "<txt>Hi</txt>", "<txt>World</txt>", "</xml>")
  Context(ctxt_query, ctxt_txt, type = "document-node()")
  expect_output(str(Execute(ctxt_query)), '"2" "5"')
  # Cleanup
  Close(Query_1)
  Close(ctxt_query)
  rm(Session)
})

test_that("Binary content is handled correctly", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  Execute(Session, "Check TestDB")
  bais <- raw()
  for (b in 252:255) bais <- c(bais, c(b)) %>% as.raw()
  Store(Session, "test.bin", bais)
  baos <- Execute(Session, "retrieve test.bin")
  expect_output(str(baos$result), "fc fd fe ff")
  # Cleanup
  Execute(Session, "Close")
  rm(Session)
})

test_that("Add and Add/Replace is handled", {
  skip_unless_socket_available()
  skip_if_offline()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  Execute(Session, "Check TestDB")
  SetIntercept(Session, TRUE)
  Execute(Session, "delete Add_Char")
  Execute(Session, "delete Add_XML")
  Execute(Session, "delete Add_URL")
  RestoreIntercept(Session)

  # Add a length-1 character vector
  Path <- "Add_Char"
  Add(Session, path = Path, "<xml>Add Char</xml>")

  # Add a file
  Path <- "Add_XML"
  XML_file <- system.file("extdata", "Articles.xml", package="RBaseX")
  Add(Session, path = Path, XML_file)

  # Add a link
  Path <- "Add_URL"
  Simple <- input_to_raw("https://raw.githubusercontent.com/BaseXdb/basex/master/basex-api/src/test/resources/first.xml")
  Add(Session, path = Path, Simple)

  # Add/Replace ----
  Rep <- "<x>Hi Friends!</x>"
  Replace(Session, Path, Rep)

  # Check results
  result <- Session$Execute("xquery db:list('TestDB')")[[1]]
  expect_equal(length(Session$Execute("xquery db:list('TestDB')")[[1]][[1]]), 7)
  expect_equal(Session$Execute("xquery doc('TestDB/Add_URL')/x/text()")[[1]][[1]], "Hi Friends!")
  # Cleanup
  Execute(Session, "Close")
  rm(Session)
})
