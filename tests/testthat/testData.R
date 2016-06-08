source("testUtils.R")

#########################
context("Distance data")
#########################

test_that("arguments are checked", {
  expect_error(distances(matrix = matrix(0, nrow = 5, ncol = 5), file = "file"), "specify either matrix or file")
  expect_error(distances(), "specify matrix or file")
  expect_error(distances(file = 123), "should be a file path")
  expect_error(distances(file = "i/do/not/exist"), "does not exist")
  expect_error(distances(matrix = "whoops"), "should be a numeric matrix")
  expect_error(distances(matrix = matrix(letters[1:25], nrow = 5, ncol = 5)), "should be a numeric matrix")
  expect_error(distances(matrix = matrix(0, nrow = 5, ncol = 5)), "names are required")
})

test_that("class is correct", {
  expect_is(distanceData(), "chdist")
})

test_that("read distance data from file", {
  dist <- distanceData()
  expect_equal(dist$file, distanceMatrixFile())
  expect_equal(getDistanceMatrix(dist), readDistanceMatrix())
})

test_that("create distance data from matrix", {
  matrix <- readDistanceMatrix()
  dist <- distances(matrix = matrix)
  expect_true(is.na(dist$file))
  expect_equal(getDistanceMatrix(dist), matrix)
})

test_that("size", {
  expect_equal(getSize(distanceData()), 100)
})

###########################
context("Core Hunter data")
###########################

test_that("arguments are checked", {
  expect_error(coreHunterData(), "specify at least one")
  expect_error(coreHunterData(genotypes = 123), "class 'chgeno'")
  expect_error(coreHunterData(phenotypes = "123"), "class 'chpheno'")
  expect_error(coreHunterData(distances = list(1, "a")), "class 'chdist'")
})

test_that("class is correct", {
  expect_is(testData(), "chdata")
})

test_that("distance matrix is correct", {
  expect_equal(getDistanceMatrix(testData()), readDistanceMatrix())
})

test_that("size", {
  expect_equal(getSize(testData()), 100)
})







