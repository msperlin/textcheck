test_that("book files functions", {

  df <- check_link("www.google.com")

  expect_true(df$status_message == "OK")

})
