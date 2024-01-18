test_that(".jpg output works", {
  expect_silent(pretty_gtable(mtcars, NULL, "output/mtcars.jpg"))
  expect_silent(pretty_gtable(mtcars, NULL, "output/mtcars.jpeg"))
})

test_that(".pdf and .png output works", {
  expect_silent(pretty_gtable(mtcars, NULL, "output/mtcars.pdf"))
  expect_silent(pretty_gtable(mtcars, NULL, "output/mtcars.png"))
})

test_that("other output fails", {
  expect_error(pretty_gtable(mtcars, NULL, "output/mtcars.pdff"), "file extension must be in \\( jpg, jpeg, pdf, png ) not pdff")
  expect_error(pretty_gtable(mtcars, NULL, "output/mtcars.pngg"), "file extension must be in \\( jpg, jpeg, pdf, png ) not pngg")
})

test_that("output files have been created", {
  for (ext in c("jpg", "jpeg", "pdf", "png")) {
    outf <- paste0("output/mtcars.", ext)
    expect_true(file.exists(outf))
    expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 20, paste(outf, "not created in last 20 seconds."))
  }
})
