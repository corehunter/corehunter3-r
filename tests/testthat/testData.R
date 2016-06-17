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
  expect_is(distanceData()$data, "matrix")
})

test_that("read distance data from file", {
  # default dataset
  dist <- distanceData()
  expect_equal(dist$file, distanceFile())
  data <- read.autodelim(distanceFile())
  data$NAME <- NULL
  matrix <- as.matrix(data)
  expect_equal(dist$data, matrix)
  expect_equal(rownames(dist$data), colnames(dist$data))
  expect_equal(rownames(dist$data), getIds())
  expect_equal(dist$names, getNames())
  # small dataset
  dist <- distanceData(dataset = "small")
  expected <- matrix(c(
    0.0, 0.2, 0.4, 0.6, 0.8,
    0.2, 0.0, 0.2, 0.4, 0.6,
    0.4, 0.2, 0.0, 0.1, 0.4,
    0.6, 0.4, 0.1, 0.0, 0.2,
    0.8, 0.6, 0.4, 0.2, 0.0
  ), nrow = 5, ncol = 5)
  rownames(expected) <- colnames(expected) <- getIds(dataset = "small")
  expect_equal(dist$data, expected)
  expect_equal(dist$names, getNames(dataset = "small"))
})

test_that("create distance data from matrix", {
  # 1: default dataset
  data <- read.autodelim(distanceFile())
  matrix <- data
  matrix$NAME <- NULL
  matrix <- as.matrix(matrix)
  # as data frame (with names)
  dist <- distances(data)
  expect_true(is.null(dist$file))
  expect_equal(dist$data, matrix)
  expect_equal(dist$names, getNames())
  # as numeric matrix (no names)
  dist <- distances(matrix)
  expect_true(is.null(dist$file))
  expect_equal(dist$data, matrix)
  expect_equal(dist$names, getIds())
  # 2: small dataset
  matrix <- matrix(c(
    0.0, 0.2, 0.4, 0.6, 0.8,
    0.2, 0.0, 0.2, 0.4, 0.6,
    0.4, 0.2, 0.0, 0.1, 0.4,
    0.6, 0.4, 0.1, 0.0, 0.2,
    0.8, 0.6, 0.4, 0.2, 0.0
  ), nrow = 5, ncol = 5)
  rownames(matrix) <- colnames(matrix) <- getIds(dataset = "small")
  # as numeric matrix (no names)
  dist <- distances(matrix)
  expect_true(is.null(dist$file))
  expect_equal(dist$data, matrix)
  expect_equal(dist$names, getIds(dataset = "small"))
  # as data frame (with names)
  data <- cbind(NAME = c(NA, NA, "Bob", "Bob", NA), as.data.frame(matrix))
  dist <- distances(data)
  expect_true(is.null(dist$file))
  expect_equal(dist$data, matrix)
  expect_equal(dist$names, getNames(dataset = "small"))
})

test_that("size", {
  expect_equal(getSize(distanceData()), 100)
})

########################
context("Genotype data")
########################

test_that("class is correct", {
  expect_is(genotypeData(), "chgeno")
  expect_is(genotypeData()$data, "data.frame")
})

test_that("read genotype data from file", {
  for(format in c("def", "bi", "freq")){
    geno <- genotypeData(format = format)
    expect_equal(geno$file, genotypeFile(format))
    expect_equal(geno$names, getNames())
    expect_equal(length(geno$alleles), geno$java$getNumberOfMarkers())
    for(m in 1:length(geno$alleles)){
      expect_equal(length(geno$alleles[[m]]), geno$java$getNumberOfAlleles(toJavaIndices(m)))
      if(format == "default"){
        expected <- unique(geno$data[[m]])
        expected <- expected[!is.na(expected)]
        expect_equal(sort(geno$alleles[[m]]), sort(expected))
      } else if(format == "bi"){
        expect_equal(geno$alleles[[m]], c("0", "1"))
      }
    }
  }
})

test_that("size", {
  expect_equal(getSize(genotypeData()), 100)
})

#########################
context("Phenotype data")
#########################

test_that("class is correct", {
  expect_is(phenotypeData(), "chpheno")
  expect_is(phenotypeData()$data, "data.frame")
})

test_that("read phenotype data from file", {
  pheno <- phenotypeData()
  expect_equal(pheno$file, phenotypeFile())
  expect_equal(pheno$names, getNames())
  # check average Gower distance of all individuals without missing data
  # (Core Hunter treats missing data slightly differently than StatMatch)
  no.missing.data <- which(!apply(is.na(pheno$data), 1, any))
  ranges <- as.numeric(apply(pheno$data, 2, max, na.rm = T)) - as.numeric(apply(pheno$data, 2, min, na.rm = T))
  gd <- StatMatch::gower.dist(pheno$data[no.missing.data, ], rngs = ranges)
  gd <- gd[lower.tri(gd)]
  expect_equal(mean(gd), evaluateCore(no.missing.data, pheno, objective("EE", "GD")))
})

test_that("size", {
  expect_equal(getSize(phenotypeData()), 100)
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
  expect_is(testData()$dist, "chdist")
  expect_is(testData()$geno, "chgeno")
  expect_is(testData()$pheno, "chpheno")
})

test_that("distance matrix is correct", {
  data <- read.autodelim(distanceFile())
  data$NAME <- NULL
  matrix <- as.matrix(data)
  expect_equal(testData()$dist$data, matrix)
})

test_that("size", {
  expect_equal(getSize(testData()), 100)
})







