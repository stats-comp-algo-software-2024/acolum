test_that('MLEs estimated via pseudo-inverse are close to truth (should return `TRUE`)', {
  sim_data <- simulate_data(n_obs = 5, n_pred = 2, model = 'linear',
                            intercept = 5,
                            coef_true = c(-4, 2.5),
                            design = as.matrix(cbind(rnorm(5, 0, 1),
                                                     rnorm(5, 10, 2))),
                            seed = 1234)
  design <- sim_data$design
  outcome <- as.matrix(design %*% sim_data$coef_true)
  expect_true(are_all_close(lm_mle_pseudoinv(design, outcome), sim_data$coef_true))
})

test_that('MLEs estimated via BFSG are close to truth (should return `TRUE`)', {
  sim_data <- simulate_data(n_obs = 5, n_pred = 2, model = 'linear',
                            intercept = 5,
                            coef_true = c(-4, 2.5),
                            design = as.matrix(cbind(rnorm(5, 0, 1),
                                                  rnorm(5, 10, 2))),
                            seed = 1234)
  design <- sim_data$design
  outcome <- as.matrix(design %*% sim_data$coef_true)
  expect_true(are_all_close(lm_mle_BFGS(design, outcome, noise_var = 0.1),
                            sim_data$coef_true))
})

test_that('The analytical and numerical gradients match (should return `TRUE`)', {
  sim_data <- simulate_data(n_obs = 5, n_pred = 2, model = 'linear',
                            seed = 1234)
  n_test <- 10
  for (i in 1:n_test) {
    beta <- rnorm(2)
    analytical_grad <- loglik_gradient(sim_data$design, sim_data$outcome, beta)
    numerical_grad <- approx_grad(function(beta) log_likelihood(sim_data$design,
                                                                sim_data$outcome,
                                                                beta),
                                  beta)
    expect_true(are_all_close(analytical_grad, numerical_grad))
  }
})

test_that('MLEs estimated via pseudo-inverse and via BFGS match (should return `TRUE`)', {
  sim_data <- simulate_data(n_obs = 5, n_pred = 2, model = 'linear',
                            seed = 1234)
  pseudoinv_result <- hiper_glm(design = sim_data$design, outcome = sim_data$outcome,
                                model = 'linear', method = 'pseudoinv')
  BFGS_result <- hiper_glm(design = sim_data$design, outcome = sim_data$outcome,
                           model = 'linear', method = 'BFGS')
  expect_true(are_all_close(v = coef(pseudoinv_result), w = coef(BFGS_result)))
})
