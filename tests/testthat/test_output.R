
test_that(".jpg output works", {
  expect_silent(pretty_gtable(mtcars, NULL, "mtcars.jpg"))
  expect_silent(pretty_gtable(mtcars, NULL, "mtcars.jpeg"))
})

test_that(".pdf and .png output works", {
  expect_silent(pretty_gtable(mtcars, NULL, "mtcars.pdf"))
  expect_silent(pretty_gtable(mtcars, NULL, "mtcars.png"))
})

test_that("other output fails", {
  expect_error(pretty_gtable(mtcars, NULL, "mtcars.pdff"), "file extension must be in \\( jpg, jpeg, pdf, png ) not pdff")
  expect_error(pretty_gtable(mtcars, NULL, "mtcars.pngg"), "file extension must be in \\( jpg, jpeg, pdf, png ) not pngg")
})

test_that("output files have been created", {
  for (ext in c("jpg", "jpeg", "pdf", "png")) {
    expect_true(file.exists(paste0("mtcars.", ext)))
  }
})
