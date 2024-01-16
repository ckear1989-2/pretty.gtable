library("data.table")

test_that("data.frame works", {
  expect_silent(pretty_gtable(mtcars, NULL))
})


test_that("data.table works", {
  expect_silent(pretty_gtable(data.table(mtcars), NULL))
})


test_that("vector doesn't work", {
  expect_error(pretty_gtable(c(1, 2, 3), NULL), "provided data should be data.frame or data.table not numeric")
})
