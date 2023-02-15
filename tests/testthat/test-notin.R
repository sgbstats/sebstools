test_that("Tests", {
  expect_equal(c(1, 3) %notin% c(1, 2), c(F, T))
})

test_that("One item", {
  expect_equal(1 %notin% c(1, 2), F)
})


test_that("One item 2", {
  expect_equal(3 %notin% c(1, 2), T)
})

test_that("String", {
  expect_equal("Foo" %notin% c("bar", "baaar"), T)
})

test_that("Vector", {
  vec <- c(1, 2)
  expect_equal(3 %notin% vec, T)
})
