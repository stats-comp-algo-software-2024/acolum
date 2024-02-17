test_that('Analytical and numerical gradients match in logistic regression (should return `TRUE`)', {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome
  set.seed(615)
  n_test <- 10
  for (i in 1:n_test) {
    beta <- rnorm(n_pred)
    analytical_grad <- logit_loglik_gradient(design, outcome, beta)
    numerical_grad <- approx_grad(function(x) logit_log_likelihood(design, outcome, x), beta)
    expect_true(are_all_close(analytical_grad, numerical_grad))
  }
})

test_that('MLEs estimated via Newton\'s method and via BFGS match in logistic regression (should return `TRUE`)', {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_newton_out <- hiper_glm(design, outcome, model = 'logit')
  via_bfgs_out <- hiper_glm(
    design, outcome, model = 'logit', option = list(mle_solver = 'BFGS')
  )
  expect_true(are_all_close(
    coef(via_newton_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
