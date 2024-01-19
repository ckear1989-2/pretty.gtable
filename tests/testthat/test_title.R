test_that("title works", {
  table.options <- list(
    rows = rownames(mtcars),
    cols = colnames(mtcars),
    width = 9,
    height = 6,
    colcs = c("red1", "red3"),
    bg_fill = "red1",
    bg_color = "black",
    bg_alpha = 0.5,
    bg_linewidth = 0,
    title = "mtcars\nfirst 16 rows"
  )
  outf <- "output/mtcars_title_bg.pdf"
  expect_silent(pretty_gtable(mtcars, table.options, outf, truncate = 1:16))
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, paste(outf, "not created in last 10 seconds."))
  table.options$bg_fill <- NULL
  outf <- "output/mtcars_title_no_bg.pdf"
  expect_silent(pretty_gtable(mtcars, table.options, outf, truncate = 1:16))
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, paste(outf, "not created in last 10 seconds."))
})
