source("testUtils.R")

########################
context("Core sampling")
########################

test_that("arguments are checked", {
  expect_error(sampleCore(), "no default")
  expect_error(sampleCore(data = "abc"), "class 'chdata'")
  data <- testData()
  expect_error(sampleCore(data, size = -1), "size")
  expect_error(sampleCore(data, size = 0), "size")
  expect_error(sampleCore(data, size = 1/getSize(data)), "size")
  expect_error(sampleCore(data, size = 1), "size")
  expect_error(sampleCore(data, size = 1.4), "size")
  expect_error(sampleCore(data, size = getSize(data)), "size")
  expect_error(sampleCore(data, size = getSize(data) + sample(1:10, size = 1)), "size")
  expect_error(sampleCore(data, obj = "abc"), "class 'chobj'")
  expect_error(sampleCore(data, obj = list(123)), "class 'chobj'")
  expect_error(sampleCore(data, obj = list(objective("SH"), objective("SH"))), "Duplicate objectives.")
  expect_error(sampleCore(data, indices = "no"), "logical")
  expect_error(sampleCore(data, verbose = "yes"), "logical")
  expect_error(sampleCore(data, time = "abc"), "numeric")
  expect_error(sampleCore(data, impr.time = "def"), "numeric")
  expect_error(sampleCore(data, time = 0), "positive")
  expect_error(sampleCore(data, impr.time = -1), "positive")
  expect_error(sampleCore(data, mode = "foo"), "one of")
})

test_that("No default objective when data contains multiple types", {
  expect_error(sampleCore(testData(), "specify at least one objective"))
})

test_that("result contains indices or names", {
  data <- testData()
  obj <- objective("EE", "PD")
  expect_true(is.character(testSampleCore(data, obj)$sel))
  expect_true(is.numeric(testSampleCore(data, obj, indices = TRUE)$sel))
})

test_that("default objective for distance data only", {
  expect_silent(testSampleCore(testData()$dist))
})

test_that("default objective for genotypes only", {
  expect_silent(testSampleCore(testData()$geno))
})

test_that("default objective for phenotypes only", {
  expect_silent(testSampleCore(testData()$pheno))
})

test_that("multiple objectives", {
  expect_silent(testSampleCore(testData(), obj = list(
    objective("AN", "PD"),
    objective("EE", "PD"),
    objective("EN", "GD")
  )))
})

test_that("core has expected class and elements", {
  data <- testData()
  # distances only
  core <- testSampleCore(data$dist)
  expect_is(core, "chcore")
  expect_false(is.null(core$sel))
  expect_false(is.null(core$EN))
  expect_false(is.null(core$EN$PD))
  expect_equal(core$EN$PD, evaluateCore(core, data$dist, objective("EN", "PD")))
  # genotypes only
  core <- testSampleCore(data$geno)
  expect_is(core, "chcore")
  expect_false(is.null(core$sel))
  expect_false(is.null(core$EN))
  expect_false(is.null(core$EN$MR))
  expect_equal(core$EN$MR, evaluateCore(core, data$geno, objective("EN", "MR")))
  # phenotypes only
  core <- testSampleCore(data$pheno)
  expect_false(is.null(core$sel))
  expect_false(is.null(core$EN))
  expect_false(is.null(core$EN$GD))
  expect_equal(core$EN$GD, evaluateCore(core, data$pheno, objective("EN", "GD")))
  # combined
  core <- testSampleCore(data, list(
    objective("EE", "PD"),
    objective("SH"),
    objective("AN", "GD")
  ))
  expect_is(core, "chcore")
  expect_false(is.null(core$sel))
  expect_false(is.null(core$EE))
  expect_false(is.null(core$EE$PD))
  expect_false(is.null(core$SH))
  expect_false(is.null(core$AN))
  expect_false(is.null(core$AN$GD))
  expect_equal(core$EE$PD, evaluateCore(core, data, objective("EE", "PD")))
  expect_equal(core$SH, evaluateCore(core, data, objective("SH")))
  expect_equal(core$AN$GD, evaluateCore(core, data, objective("AN", "GD")))
})

#####################
context("Objectives")
#####################

test_that("class is correct", {
  obj <- objective()
  expect_is(obj, "chobj")
})







