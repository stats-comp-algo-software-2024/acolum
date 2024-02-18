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

test_that('Analytical and numerical Hessians match in logistic regression (should return `TRUE`)', {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design
  outcome <- data$outcome
  set.seed(615)
  n_test <- 10
  for (i in 1:n_test) {
    v <- rnorm(n_pred)
    beta <- rnorm(n_pred)
    eps <- 1e-8
    hess_v_approx <- (logit_loglik_gradient(design, outcome, beta + eps * v) - logit_loglik_gradient(design, outcome, beta - eps * v)) / (2 * eps)
    hess_v <- logit_hessian(design, beta) %*% v
    expect_true(are_all_close(as.vector(hess_v_approx), as.vector(hess_v)))
  }
})

test_that('MLEs estimated via Newton\'s method and via BFGS match in logistic regression (should return `TRUE`)', {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_newton_out <- hiper_glm(design, outcome, model = 'logit', method = 'newton')
  via_bfgs_out <- hiper_glm(design, outcome, model = 'logit', method = 'BFGS')
  expect_true(are_all_close(
    coef(via_newton_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
