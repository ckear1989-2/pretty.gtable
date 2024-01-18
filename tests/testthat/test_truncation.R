test_that("truncation works (with warnings)", {
  table.options <- list(
    rows = rownames(mtcars),
    cols = colnames(mtcars),
    width = 9,
    height = 4
  )
  expect_silent(pretty_gtable(mtcars, table.options, "output/mtcars_16.pdf", truncate = 1:16))
  expect_silent(pretty_gtable(mtcars, table.options, "output/mtcars_mazda.pdf", truncate = c("Mazda RX4", "Mazda RX4 Wag")))
  expect_warning(pretty_gtable(mtcars, table.options, "output/mtcars_fakecar.pdf", truncate = c("fakecar1", "fakecar2")), "attempting to truncate data with incompatible indexing")
  expect_warning(pretty_gtable(mtcars, table.options, "output/mtcars_1000.pdf", truncate = 1:1000), "attempting to truncate data with incompatible indexing")
})
