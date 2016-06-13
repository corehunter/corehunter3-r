source("testUtils.R")

#########################
context("Distance data")
#########################

test_that("arguments are checked", {
  expect_error(distances(matrix(0, nrow = 5, ncol = 5), file = "file"), "specify either matrix or file")
  expect_error(distances(), "specify matrix or file")
  expect_error(distances(file = 123), "should be a file path")
  expect_error(distances(file = "i/do/not/exist"), "does not exist")
  expect_error(distances(data = "whoops"), "a matrix or a data frame")
  expect_error(distances(matrix(letters[1:25], nrow = 5, ncol = 5)), "should be numeric")
  expect_error(distances(matrix(0, nrow = 5, ncol = 5)), "names are required")
})

test_that("class is correct", {
  expect_is(distanceData(), "chdist")
})

test_that("read distance data from file", {
  dist <- distanceData()
  expect_equal(dist$file, distanceFile())
  expect_equal(dist$data, readDistanceMatrix())
})

test_that("read genotype data from file", {
  geno <- genotypeData()
  expect_equal(geno$file, genotypeFile())
})

test_that("read phenotype data from file", {
  pheno <- phenotypeData()
  expect_equal(pheno$file, phenotypeFile())
})

test_that("create distance data from matrix", {
  matrix <- readDistanceMatrix()
  # as data frame
  dist <- distances(matrix)
  expect_true(is.null(dist$file))
  expect_equal(dist$data, matrix)
  # as numeric matrix
  matrix$NAME <- NULL
  matrix <- as.matrix(matrix)
  dist <- distances(matrix)
  expect_true(is.null(dist$file))
  expect_equal(dist$data, matrix)
})

test_that("size", {
  expect_equal(getSize(distanceData()), 100)
})

###########################
context("Core Hunter data")
###########################

test_that("arguments are checked", {
  expect_error(coreHunterData(), "specify at least one")
  expect_error(coreHunterData(distances = list(1, "a")), "class 'chdist'")
  expect_error(coreHunterData("x"), "class 'chdist'")
  expect_error(coreHunterData(genotypes = 123), "class 'chgeno'")
  expect_error(coreHunterData(phenotypes = "123"), "class 'chpheno'")
})

test_that("class is correct", {
  expect_is(testData(), "chdata")
  expect_is(distanceData(), "chdist")
  expect_is(genotypeData(), "chgeno")
  expect_is(phenotypeData(), "chpheno")
})

test_that("distance matrix is correct", {
  expect_equal(testData()$dist$data, readDistanceMatrix())
})

test_that("size", {
  expect_equal(getSize(testData()), 100)
})







