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
})







