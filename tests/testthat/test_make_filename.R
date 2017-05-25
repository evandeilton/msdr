context("Check some function from package")

test_that("Mapping if file name input is identical in make_filename() ", {
  path <- "2013"
  x <- make_filename(path)
  class(x)
  head(x)
  expect_identical(x, "accident_2013.csv.bz2")
})