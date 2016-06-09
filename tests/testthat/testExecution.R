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

#####################
context("Objectives")
#####################

test_that("class is correct", {
  obj <- objective()
  expect_is(obj, "chobj")
})







