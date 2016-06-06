context("distances data")
print(dir())
.jinit(Sys.glob("../../inst/java/*.jar"))

test_that("arguments are checked", {
  expect_error(ch.distances(matrix = matrix(0, nrow = 5, ncol = 5), file = "file"), "specify either matrix or file")
  expect_error(ch.distances(), "specify matrix or file")
  expect_error(ch.distances(file = 123), "should be a file path")
  expect_error(ch.distances(file = "i/do/not/exist"), "does not exist")
  expect_error(ch.distances(matrix = "whoops"), "should be a numeric matrix")
  expect_error(ch.distances(matrix = matrix(letters[1:25], nrow = 5, ncol = 5)), "should be a numeric matrix")
  expect_error(ch.distances(matrix = matrix(0, nrow = 5, ncol = 5)), "names are required")
})

