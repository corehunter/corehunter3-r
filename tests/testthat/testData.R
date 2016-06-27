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
  m <- matrix(1:25, nrow = 5, ncol = 5)
  rownames(m) <- colnames(m) <- 1:5
  expect_error(distances(m), "matrix should be symmetric")
})

test_that("class", {
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
  expect_equal(dist$size, 100)
  expect_equal(dist$data, matrix)
  expect_equal(dist$ids, getIds())
  expect_equal(rownames(dist$data), dist$ids)
  expect_equal(colnames(dist$data), dist$ids)
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
  expect_equal(dist$size, 5)
  expect_equal(dist$ids, getIds(dataset = "small"))
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
  expect_equal(dist$size, 100)
  expect_equal(dist$data, matrix)
  expect_equal(dist$ids, getIds())
  expect_equal(dist$names, getNames())
  # as numeric matrix (no explicit names)
  dist <- distances(matrix)
  expect_true(is.null(dist$file))
  expect_equal(dist$size, 100)
  expect_equal(dist$data, matrix)
  expect_equal(dist$ids, getIds())
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
  # as numeric matrix (no explicit names)
  dist <- distances(matrix)
  expect_true(is.null(dist$file))
  expect_equal(dist$size, 5)
  expect_equal(dist$data, matrix)
  expect_equal(dist$ids, getIds(dataset = "small"))
  expect_equal(dist$names, getIds(dataset = "small"))
  # as data frame (no explicit names)
  dist <- distances(as.data.frame(matrix))
  expect_true(is.null(dist$file))
  expect_equal(dist$size, 5)
  expect_equal(dist$data, matrix)
  expect_equal(dist$ids, getIds(dataset = "small"))
  expect_equal(dist$names, getIds(dataset = "small"))
  # as data frame (with names)
  data <- cbind(NAME = c(NA, NA, "Bob", "Bob", NA), as.data.frame(matrix))
  dist <- distances(data)
  expect_true(is.null(dist$file))
  expect_equal(dist$size, 5)
  expect_equal(dist$data, matrix)
  expect_equal(dist$ids, getIds(dataset = "small"))
  expect_equal(dist$names, getNames(dataset = "small"))

})

test_that("print", {
  data <- distanceData()
  expect_output(print(data), "Precomputed distance matrix for 100 individuals.")
})

########################
context("Genotype data")
########################

test_that("arguments are checked", {
  expect_error(genotypes(), "path is required")
  expect_error(genotypes(file = 124), "should be a file path")
  expect_error(genotypes(file = "i/do/not/exist"), "does not exist")
})

test_that("class", {
  expect_is(genotypeData(), "chgeno")
  expect_is(genotypeData()$data, "data.frame")
})

test_that("read genotype data from file", {
  # 1: default dataset
  for(format in c("def", "bi", "freq")){
    geno <- genotypeData(format = format)
    expect_equal(geno$file, genotypeFile(format))
    expect_equal(geno$size, 100)
    expect_equal(geno$ids, getIds())
    expect_equal(rownames(geno$data), geno$ids)
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
  # 2: small dataset
  geno <- genotypeData(format = "default", dataset = "small")
  expect_equal(geno$size, 5)
  expect_equal(geno$ids, getIds(dataset = "small"))
  expect_equal(rownames(geno$data), geno$ids)
  expect_equal(geno$names, getNames(dataset = "small"))
  expect_equal(length(geno$alleles), 4)
  expect_equal(geno$alleles[[1]], c("1", "2", "3"))
  expect_equal(geno$alleles[[2]], c("A", "B", "C", "D"))
  expect_equal(geno$alleles[[3]], c("a1", "a2"))
  expect_equal(geno$alleles[[4]], c("+", "-"))
})

test_that("print", {
  data <- genotypeData()
  expect_output(print(data), "Genotypes for 100 individuals \\(18 markers\\).")
})

#########################
context("Phenotype data")
#########################

test_that("arguments are checked", {
  expect_error(phenotypes(), "path is required")
  expect_error(phenotypes(file = 124), "should be a file path")
  expect_error(phenotypes(file = "i/do/not/exist"), "does not exist")
})

test_that("class", {
  expect_is(phenotypeData(), "chpheno")
  expect_is(phenotypeData()$data, "data.frame")
})

test_that("read phenotype data from file", {
  # 1: default dataset
  pheno <- phenotypeData()
  expect_equal(pheno$file, phenotypeFile())
  expect_equal(pheno$size, 100)
  expect_equal(pheno$ids, getIds())
  expect_equal(rownames(pheno$data), pheno$ids)
  expect_equal(pheno$names, getNames())
  expect_equal(pheno$ranges, getRanges())
  # check average Gower distance of all individuals without missing data
  # (Core Hunter treats missing data slightly differently than StatMatch)
  no.missing.data <- which(!apply(is.na(pheno$data), 1, any))
  gd <- StatMatch::gower.dist(pheno$data[no.missing.data, ], rngs = pheno$ranges)
  gd <- gd[lower.tri(gd)]
  expect_equal(mean(gd), evaluateCore(no.missing.data, pheno, objective("EE", "GD")))
  # 2: small dataset
  pheno <- phenotypeData(dataset = "small")
  expect_equal(pheno$size, 5)
  expect_equal(pheno$ids, getIds(dataset = "small"))
  expect_equal(rownames(pheno$data), pheno$ids)
  expect_equal(pheno$names, getNames(dataset = "small"))
  expect_equal(pheno$ranges, getRanges(dataset = "small"))
  # check average Gower distance (no missing data in small dataset)
  gd <- StatMatch::gower.dist(pheno$data, rngs = pheno$ranges)
  gd <- gd[lower.tri(gd)]
  expect_equal(mean(gd), evaluateCore(1:5, pheno, objective("EE", "GD")))
})

test_that("create phenotype data from data frame", {
  df <- data.frame(
    n = sample(letters[1:10], size = 5, replace = TRUE),
    i = sample(1:10, size = 5, replace = TRUE),
    o = ordered(sample(letters[1:10], size = 5, replace = TRUE), levels = letters[1:10]),
    r = rnorm(5),
    b = sample(c(T,F), size = 5, replace = TRUE)
  )
  # TODO ...
})

test_that("print", {
  data <- phenotypeData()
  expect_output(print(data), "Phenotypes for 100 individuals \\(39 traits\\).")
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

test_that("class", {
  expect_is(testData(), "chdata")
  expect_is(testData()$dist, "chdist")
  expect_is(testData()$geno, "chgeno")
  expect_is(testData()$pheno, "chpheno")
})

test_that("distance matrix", {
  data <- read.autodelim(distanceFile())
  data$NAME <- NULL
  matrix <- as.matrix(data)
  expect_equal(testData()$dist$data, matrix)
})

test_that("size", {
  expect_equal(testData()$size, 100)
})

test_that("example data", {
  data <- exampleData()
  expect_equal(data$dist, distanceData())
  expect_equal(data$geno, genotypeData())
  expect_equal(data$pheno, phenotypeData())
})

test_that("print", {
  data <- testData()
  expect_output(print(data), "Core Hunter data containing genotypes, phenotypes & precomputed distances for 100 individuals.")
  data <- coreHunterData(geno = genotypeData())
  expect_output(print(data), "Core Hunter data containing genotypes for 100 individuals.")
  data <- coreHunterData(pheno = phenotypeData())
  expect_output(print(data), "Core Hunter data containing phenotypes for 100 individuals.")
  data <- coreHunterData(dist = distanceData())
  expect_output(print(data), "Core Hunter data containing precomputed distances for 100 individuals.")
})





