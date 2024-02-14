test_that('`are_all_close()` correctly returns `TRUE` when the inputs are close', {
  v <- c(10, 10)
  w <- v + 1e-8
  expect_true(are_all_close(v = v, w = w))
})


test_that('`are_all_close()` correctly returns `FALSE` when the relative error is above `rel_tol`', {
  v <- c(0.1, 0.1)
  w <- v + 1e-2
  expect_false(are_all_close(v = v, w = w, abs_tol = 1e-2, rel_tol = 1e-8))
})


test_that('`are_all_close()` correctly returns `FALSE` when the absolute error is above `abs_tol`', {
  v <- c(0.1, 0.1)
  w <- v + 1e-5
  expect_false(are_all_close(v = v, w = w, abs_tol = 1e-8, rel_tol = 1e-2))
})
