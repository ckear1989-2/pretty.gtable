
test_that(".jpg output works", {
  expect_silent(pretty_gtable(mtcars, NULL, "mtcars.jpg"))
})

