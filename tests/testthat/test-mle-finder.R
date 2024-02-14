test_that('MLEs estimated via pseudo-inverse are close to truth (should return `TRUE`)', {
  set.seed(1234)
  design <- as.matrix(cbind(rep(1, 5), rnorm(5, 0, 1), rnorm(5, 10, 2)))
  truth <- as.matrix(c(5, -4, 2.5))
  outcome <- as.matrix(design %*% truth)
  expect_true(are_all_close(lm_mle_pseudoinv(design, outcome), truth))
})

test_that('MLEs estimated via BFSG are close to truth (should return `TRUE`)', {
  set.seed(1234)
  design <- as.matrix(cbind(rep(1, 5), rnorm(5, 0, 1), rnorm(5, 10, 2)))
  truth <- as.matrix(c(5, -4, 2.5))
  outcome <- as.matrix(design %*% truth)
  expect_true(are_all_close(lm_mle_BFGS(design, outcome, noise_var = 0.1),
                            truth))
})

test_that('The analytical and numerical gradients match (should return `TRUE`)', {
  set.seed(1234)
  design <- as.matrix(cbind(rep(1, 5), rnorm(5, 0, 1), rnorm(5, 10, 2)))
  truth <- as.matrix(c(5, -4, 2.5))
  outcome <- as.matrix(design %*% truth)
  n_test <- 10
  grads_are_close <- TRUE
  for (i in 1:n_test) {
    if (!grads_are_close) break
    beta <- rnorm(3)
    analytical_grad <- loglik_gradient(design, outcome, beta)
    numerical_grad <- approx_grad(function(beta) log_likelihood(design, outcome, beta), beta)
    grads_are_close <- are_all_close(analytical_grad, numerical_grad)
  }
  expect_true(grads_are_close)
})

test_that('MLEs estimated via pseudo-inverse and via BFGS match (should return `TRUE`)', {
  set.seed(1234)
  design <- as.matrix(cbind(rep(1, 5), rnorm(5, 0, 1), rnorm(5, 10, 2)))
  truth <- as.matrix(c(5, -4, 2.5))
  outcome <- as.matrix(design %*% truth)
  pseudoinv_result <- hiper_glm(design = design, outcome = outcome,
                           model = 'linear', method = 'pseudoinv')
  BFGS_result <- hiper_glm(design = design, outcome = outcome,
                           model = 'linear', method = 'BFGS')
  expect_true(are_all_close(v = coef(pseudoinv_result), w = coef(BFGS_result)))
})
