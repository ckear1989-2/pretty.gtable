test_that("test file for available fonts", {
  for (font in names(pdfFonts())) {
    outf <- paste0("output/mtcars_", font, ".pdf")
    expect_silent(pretty_gtable(mtcars, list(fontfamily = font, rows = rownames(mtcars), cols = colnames(mtcars)), outf, truncate = 1:16))
    expect_true(file.exists(outf))
    expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 20, paste(outf, "not created in last 20 seconds."))
  }
})
