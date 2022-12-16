test_that("book files functions", {

  example_file <- fs::path(
    system.file('extdata/examples/example_RMD.Rmd', package = 'textcheck')
  )

  flag <- find_todos(example_file)

  expect_true(flag)

})
