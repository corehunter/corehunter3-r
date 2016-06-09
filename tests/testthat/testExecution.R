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
  expect_error(sampleCore(data, silent = "yes"), "logical")
  expect_error(sampleCore(data, time = "abc"), "numeric")
  expect_error(sampleCore(data, impr.time = "def"), "numeric")
  expect_error(sampleCore(data, time = 0), "positive")
  expect_error(sampleCore(data, impr.time = -1), "positive")
  expect_error(sampleCore(data, mode = "foo"), "one of")
})

test_that("No default objective when data contains multiple types", {
  # ...
})

test_that("result contains indices or names", {
  data <- testData()
  expect_true(is.character(testSampleCore(data)$sel))
  expect_true(is.numeric(testSampleCore(data, indices = TRUE)$sel))
})

test_that("default objective for distance data only", {
  expect_silent(testSampleCore(testData()))
})

test_that("explicit objective", {
  expect_silent(testSampleCore(testData(), obj = objective("EE", "PD")))
})

test_that("multiple objectives", {
  expect_silent(testSampleCore(testData(), obj = list(objective("AN", "PD"), objective("EE", "PD"))))
})

test_that("core has expected class and elements", {
  core <- testSampleCore(testData())
  expect_is(core, "chcore")
  expect_false(is.null(core$sel))
  expect_false(is.null(core$EN))
  expect_false(is.null(core$EN$PD))
  expect_equal(core$EN$PD, evaluateCore(core, testData(), objective("EN", "PD")))
})

#####################
context("Objectives")
#####################

test_that("class is correct", {
  obj <- objective()
  expect_is(obj, "chobj")
})







