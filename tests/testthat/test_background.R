test_that("background colorisation works", {
  table.options <- list(
    rows = rownames(mtcars),
    cols = colnames(mtcars),
    width = 9,
    height = 4,
    colcs = c("red1", "red3"),
    bg_fill = "grey90",
    bg_color = "black",
    bg_alpha = 0.5,
    bg_linewidth = 0
  )
  outf <- "output/mtcars_16_bg_fill_grey.pdf"
  expect_silent(pretty_gtable(mtcars, table.options, outf, truncate = 1:16))
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, paste(outf, "not created in last 10 seconds."))
  table.options$colcs <- c("gold1", "gold3")
  table.options$bg_fill <- "red"
  outf <- "output/mtcars_16_bg_fill_red.pdf"
  expect_silent(pretty_gtable(mtcars, table.options, outf, truncate = 1:16))
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, paste(outf, "not created in last 10 seconds."))
})
