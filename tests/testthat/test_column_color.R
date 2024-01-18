test_that("color colorisation works", {
  table.options <- list(
    rows = rownames(mtcars),
    cols = colnames(mtcars),
    width = 9,
    height = 4,
    colcs = c("red1", "red3")
  )
  outf <- "output/mtcars_16_col_color_red.pdf"
  expect_silent(pretty_gtable(mtcars, table.options, outf, truncate = 1:16))
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, paste(outf, "not created in last 10 seconds."))
  table.options$colcs <- c("gold1", "gold3")
  outf <- "output/mtcars_16_col_color_gold.pdf"
  expect_silent(pretty_gtable(mtcars, table.options, outf, truncate = 1:16))
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, paste(outf, "not created in last 10 seconds."))
})
