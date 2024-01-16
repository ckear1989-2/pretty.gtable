
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

test_that("truncation works (with warnings)", {
  expect_silent(pretty_gtable(mtcars, NULL, "mtcars_16.pdf", truncate=1:16))
  expect_silent(pretty_gtable(mtcars, NULL, "mtcars_mazda.pdf", truncate=c("Mazda RX4", "Mazda RX4 Wag")))
  expect_warning(pretty_gtable(mtcars, NULL, "mtcars_fakecar.pdf", truncate=c("fakecar1", "fakecar2")), "attempting to truncate data with incompatible indexing")
  expect_warning(pretty_gtable(mtcars, NULL, "mtcars_1000.pdf", truncate=1:1000), "attempting to truncate data with incompatible indexing")
})
