test_that("Create Single File", {
  skip_unless_socket_available()

  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")
  Session$set_intercept(TRUE)
  Session$set_success(FALSE)  # Initialize success

  DB_1 <- "Single_1"
  Single_1 <- "<Line_1 line='1'>Content 1</Line_1>"
  Session$Execute(paste("drop DB", DB_1))
  Session$Create(DB_1, Single_1)
  expect_true(GetSuccess(Session))
  Session$Execute(paste("drop DB", DB_1))

  DB_2 <- "Single_2"
  Single_2 <- paste(system.file("extdata", "xml_files", package="RBaseX"), "h-tk-20202021-102-12.xml", sep="/")
  Session$Execute(paste("drop DB", DB_2))
  Session$Create(DB_2, Single_2)
  expect_true(GetSuccess(Session))
  Session$Execute(paste("drop DB", DB_2))

})

test_that("Parl_Test is created and queried", {
  skip_unless_socket_available()
  Session <- BasexClient$new("localhost", 1984L, username = "admin", password = "admin")

  #DB_Name <- "Parliament"
  DB_Name <- "Parl_Test"
  XML_Files <- system.file("extdata", "xml_files", package="RBaseX")

  Session$set_intercept(TRUE)
  Session$set_success(FALSE)  # Initialize success
  Session$Command(as.character(glue("Open {DB_Name}")))
  Opened <- Session$get_success()
  if (!Opened) {
    Session$Command(as.character(glue("Create db {DB_Name} {XML_Files}")))
  }

  Query_Expr <- paste(
    'import module namespace  functx = "http://www.functx.com";',
    'for $Debat in collection("Parl_Test")',
    '  let $debate-id := fn:analyze-string(',
    '    $Debat/officiele-publicatie/metadata/meta/@content, "(\\d{8}-\\d*-\\d*)")//fn:match/*:group[@nr="1"]/text()',
    '  for $Beurt at $CountInner in $Debat//spreekbeurt',
    '    let $tekst := fn:string-join($Beurt//al/text(), "&#13;")',
    'order by $debate-id',
    'return($debate-id, $CountInner, $tekst)')

  QueryExe <- Session$Query(Query_Expr)
  result <- QueryExe$queryObject$ExecuteQuery()
  result_frame <- result2frame(result, 3)
  expect_equal(nrow(result_frame), 699)

  rm(Session)
})
