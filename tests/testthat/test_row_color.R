test_that("row colorisation works", {
  table.options <- list(
    rows = rownames(mtcars),
    cols = colnames(mtcars),
    width = 9,
    height = 4,
    rowcs = c("red1", "red3")
  )
  outf <- "output/mtcars_16_row_color_red.pdf"
  expect_silent(pretty_gtable(mtcars, table.options, outf, truncate = 1:16))
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, paste(outf, "not created in last 10 seconds."))
  table.options$rowcs <- c("gold1", "gold3")
  outf <- "output/mtcars_16_row_color_gold.pdf"
  expect_silent(pretty_gtable(mtcars, table.options, outf, truncate = 1:16))
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, paste(outf, "not created in last 10 seconds."))
})

test_that("row colorisation works for one or several clors", {
  table.options <- list(
    rows = rownames(mtcars),
    cols = colnames(mtcars),
    width = 9,
    height = 4,
    rowcs = "red1"
  )
  outf <- "output/mtcars_16_row_color_all_red.pdf"
  expect_silent(pretty_gtable(mtcars, table.options, outf, truncate = 1:16))
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, paste(outf, "not created in last 10 seconds."))
  table.options$rowcs <- c("red", "orange", "yellow", "green", "blue", "purple", "violet")
  outf <- "output/mtcars_16_row_color_rainbow.pdf"
  expect_silent(pretty_gtable(mtcars, table.options, outf, truncate = 1:16))
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, paste(outf, "not created in last 10 seconds."))
})
