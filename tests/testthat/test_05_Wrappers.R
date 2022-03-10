test_that("Session, Database and QueryObjects are created and can be used", {
  skip_unless_socket_available()
  Session <- NewBasexClient(user = "admin", password = "admin")

  SetIntercept(Session, FALSE)
  expect_named(Command(Session, "Info"))
  expect_error(Query(Session))
  SetIntercept(Session, TRUE)
  expect_true(GetIntercept(Session))
  Command(Session, "Open TestOpen")        # Ensure that TestOpen does not exist
  if (GetSuccess(Session)) {
    Command(Session,"DROP DB TestOpen")
  }
  SetIntercept(Session, FALSE)             # should be FALSE
  expect_error(Command(Session, "Open TestOpen"))
  SetIntercept(Session, TRUE)
  Command(Session, "Open TestOpen")
  if (!GetSuccess(Session)) {
    Create(Session, "TestOpen")
  }
  Command(Session, "Open TestOpen")
  expect_true(GetSuccess(Session))
  Command(Session, "Close")
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

  Command(Session,"DROP DB TestOpen")

  # Cleanup
  Close(Query_2)
  Close(Query_3)
  rm(Session)
})

test_that("Full Query", {
  skip_unless_socket_available()
  Session <- NewBasexClient(user = "admin", password = "admin")

  Query_6 <- Query(Session, "collection('/TestDB/Test.xml')")
  fullResult <- Full(Query_6)
  expect_length(fullResult$fullResult, 6)
  expect_equal(fullResult$fullResult[[1]][[2]], "/TestDB/Test.xml")

  Zero_results <- Query(Session, "collection('TestDB/Test')")
  fullResult <- Full(Zero_results)
  expect_length(fullResult$fullResult, 0)

  # Cleanup
  Close(Query_6)
  Close(Zero_results)
  rm(Session)
})

test_that("More and iterate", {
  skip_unless_socket_available()
  Session <- NewBasexClient(user = "admin", password = "admin")

  Query_iter <- Query(Session, "collection('/TestDB/Test.xml')")
  Zero_results <- Query(Session, "collection('TestDB/Test')")

  iterResult <- c()
  while (More(Query_iter)) {
    iterResult <- c(iterResult, Next(Query_iter))
  }
  expect_length(iterResult, 3)

  iterResult <- c()
  while (More(Zero_results)) {
    iterResult <- c(iterResult, Next(Zero_results))
  }
  expect_length(iterResult, 0)

  # Cleanup
  Close(Query_iter)
  Close(Zero_results)
  rm(Session)
})

test_that("Query is executed and Updating() is false", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  Query_1 <- Query(Session, "for $i in 1 to 2 return <xml>Text { $i }</xml>")
  expect_equal(Session$get_success(), TRUE)
  Query_2 <- Query(Session, "for $i in 3 to 4 return $i")
  expect_equal(Session$get_success(), TRUE)
  res <- Command(Query_1)
  info <- Info(Query_1)
  expect_length(res[[1]], 2)
  expect_equal(substr(info$Info, 1, 5), "Query")

  res <- Command(Query_2)
  t2 <- c("3", "4")
  expect_equal(t2, res$Result)

  expect_equal(Updating(Query_1)$result, "false")
  expect_equal(Options(Query_1)$Options, "No options set")

  # Cleanup
  Close(Query_1)
  Close(Query_2)
  rm(Session)
})

test_that("Binding function binds variables", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  Query_1 <- Query(Session,
                   "declare variable $name external; for $i in 1 to 2 return element { $name } { $i }")
  Bind(Query_1, "$name", "number")
  expect_output(str(Command(Query_1)), '"<number>1</number>" "<number>2</number>"')

  Query_2 <- Query(Session,
                   "declare variable $name external; for $i in 3 to 4 return element { $name } { $i }")
  Bind(Query_2, "$name", "number", "xs:string")
  expect_output(str(Command(Query_2)), '"<number>3</number>" "<number>4</number>"')

  Query_3 <- Query(Session, "declare variable $name external;
    for $t in collection('TestDB/Books')/book
    where $t/@author = $name
    return $t/@title/string()")
  names <- list("Walmsley", "Wickham")
  Bind(Query_3, "$name", names)
  expect_output(str(Command(Query_3)), '"XQuery" "Advanced R"')

  Query_4 <- Query(Session, "declare variable $name external;
    for $t in collection('TestDB/Books')/book
    where $t/@author = $name
    return $t/@title/string()")
  types <- list("xs:string", "xs:string")
  Bind(Query_4, "$name", names, types)
  expect_output(str(Command(Query_4)), '"XQuery" "Advanced R"')

  # Cleanup
  Close(Query_1)
  Close(Query_2)
  Close(Query_3)
  Close(Query_4)
  rm(Session)
})

test_that("Context function works", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  ctxt_query <- Query(Session, "for $t in .//text() return string-length($t)")
  ctxt_txt   <- paste0("<xml>", "<txt>Hi</txt>", "<txt>World</txt>", "</xml>")
  t <- Context(ctxt_query, ctxt_txt, type = "document-node()")
  expect_output(str(Command(ctxt_query)), '"2" "5"')

  # Cleanup
  Close(ctxt_query)
  rm(Session)
})

test_that("Binary content is handled correctly", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  Command(Session, "Check TestDB")
  bais <- as.raw(c(252, 253 ,254, 255, 255, 254, 255, 000, 255, 252, 253 ,254, 255, 255, 254, 255, 000, 255))
  Store(Session, "test.bin", bais)

  baos <- Command(Session, "retrieve test.bin")
  expect_equal(baos$result, as.raw(c(252, 253 ,254, 255, 255, 254, 255, 000, 255, 252, 253 ,254, 255, 255, 254, 255, 000, 255)))
  # Cleanup
  Command(Session, "Close")
  rm(Session)
})

test_that("Add and Add/Replace is handled", {
  skip_unless_socket_available()
  skip_if_offline()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  Command(Session, "Check TestDB")
  SetIntercept(Session, TRUE)
  Command(Session, "delete Add_Char")
  Command(Session, "delete Add_XML")
  Command(Session, "delete Add_URL")
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

  # Add/Replace
  Rep <- "<x>Hi Friends!</x>"
  Replace(Session, Path, Rep)

  # Check results
  result <- Session$Command("xquery db:list('TestDB')")[[1]]
  expect_equal(length(Session$Command("xquery db:list('TestDB')")[[1]][[1]]), 8)
  expect_equal(Session$Command("xquery doc('TestDB/Add_URL')/x/text()")[[1]][[1]], "Hi Friends!")
  # Cleanup
  Command(Session, "Close")
  rm(Session)
})
