test_that("Inverse is correct", {
  x <- matrix(c(1, -1, 0, 4, 5, 0, 2, 1, 1), 3, 3)
  x_inv <- 1/9 * matrix(c(5, 1, 0, -4, 1, 0, -6, -3, 9), 3, 3)
  x_c <- makeCacheMatrix(x)
  # Known inverse
  expect_equal(cacheSolve(x_c), x_inv)
})

test_that("Caching is performed", {
  x <- matrix(rnorm(16, 2, 0.5), 4, 4)
  x_c <- makeCacheMatrix(x)
  # No cache
  x_inv <- expect_silent(cacheSolve(x_c))
  expect_equal(x_inv, solve(x))
  # Cache
  x_inv <- expect_message(cacheSolve(x_c), regexp = "getting cached data")
  x_inv <- expect_message(cacheSolve(x_c), regexp = "getting cached data")
  expect_equal(x_inv, solve(x))
})
