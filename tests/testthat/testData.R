.jinit(Sys.glob("../../inst/java/*.jar"))

#########################
context("Distance data")
#########################

# utility functions
distanceMatrixFile <- function(){
  system.file("extdata", "distances.csv", package = "corehunter")
}
readDistanceMatrix <- function(){
  file <- distanceMatrixFile()
  matrix <- as.matrix(read.csv(file, row.names = 1, header = T))
  colnames(matrix) <- rownames(matrix)
  return(matrix)
}

test_that("arguments are checked", {
  expect_error(distances(matrix = matrix(0, nrow = 5, ncol = 5), file = "file"), "specify either matrix or file")
  expect_error(distances(), "specify matrix or file")
  expect_error(distances(file = 123), "should be a file path")
  expect_error(distances(file = "i/do/not/exist"), "does not exist")
  expect_error(distances(matrix = "whoops"), "should be a numeric matrix")
  expect_error(distances(matrix = matrix(letters[1:25], nrow = 5, ncol = 5)), "should be a numeric matrix")
  expect_error(distances(matrix = matrix(0, nrow = 5, ncol = 5)), "names are required")
})

test_that("read distance data from file", {
  dist <- distances(file = distanceMatrixFile())
  expect_equal(dist$file, distanceMatrixFile())
  expect_equal(getDistanceMatrix(dist), readDistanceMatrix())
})

test_that("create distance data from matrix", {
  matrix <- readDistanceMatrix()
  dist <- distances(matrix = matrix)
  expect_true(is.na(dist$file))
  expect_equal(getDistanceMatrix(dist), matrix)
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







