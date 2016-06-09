source("testUtils.R")

##########################
context("Core evaluation")
##########################

test_that("arguments are checked", {
  expect_error(evaluateCore(TRUE), "no applicable method")
  expect_error(evaluateCore(1:10), "missing")
  expect_error(evaluateCore(1:10, "data"), "'chdata'")
  expect_error(evaluateCore(1:10, testData(), "obj"), "'chobj'")
  expect_error(evaluateCore(123456, testData(), objective("EN", "PD")), "unable to select")
})

test_that("larger cores are more representative but have higher redundancy", {
  data <- testData()
  core.small <- testSampleCore(data, size = 5)
  core.large <- testSampleCore(data, size = 95)
  objs <- list(
    objective("EN"),
    objective("EE"),
    objective("AN")
  )
  for(obj in objs){
    expect_gte(evaluateCore(core.small, data, obj), evaluateCore(core.large, data, obj))
  }
})
