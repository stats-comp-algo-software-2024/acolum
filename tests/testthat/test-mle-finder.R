test_that("MLEs estimated via pseudo-inverse and via BFGS match (should return `TRUE`)", {
  set.seed(1234)
  design <- as.matrix(cbind(rep(1, 5), rnorm(5, 0, 1), rnorm(5, 10, 2)))
  truth <- as.matrix(c(5, -4, 2.5))
  outcome <- as.matrix(design %*% truth)
  pinv_result <- hiper_glm(design, outcome, model = "linear", method = "pinv")
  BFGS_result <- hiper_glm(design, outcome, model = "linear", method = "BFGS")
  expect_true(are_all_close(coef(pinv_result), coef(BFGS_result),
                            abs_tol = 1e-3, rel_tol = 1e-3
  ))
})
