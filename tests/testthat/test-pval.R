test_that("No Stars", {
  expect_equal(pval(0.06, addstars = c(0.05, 0.01)), "0.060")
})

test_that("One star", {
  expect_equal(pval(0.02, addstars = c(0.05, 0.01)), "0.020*")
})

test_that("Two Stars", {
  expect_equal(pval(0.005, addstars = c(0.05, 0.01)), "0.005**")
})

test_that("Less than", {
  expect_equal(pval(0.0005), "<0.001")
})

test_that("Less than 2", {
  expect_equal(pval(0.005, lessthan = 0.01), "<0.010")
})

test_that("Format", {
  expect_equal(pval(0.005, lessthan = 0.0001, format = "%.4f"), "0.0050")
})

test_that("Decreasing order", {
  expect_error(pval(0.02, addstars = c(0.01, 0.05)), "Values for stars must be in decreasing order")
})
