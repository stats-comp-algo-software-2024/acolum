are_all_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  are_all_within_atol <- all(abs_diff < abs_tol)
  are_all_within_rtol <- all(abs_diff < rel_tol * pmax(abs(v), abs(w)))
  return(are_all_within_atol && are_all_within_rtol)
}

simulate_data <- function(
    n_obs, n_pred, model = 'linear', intercept = NULL,
    coef_true = NULL, design = NULL, seed = NULL, signal_to_noise = 0.1
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (is.null(coef_true)) {
    coef_true <- rnorm(n_pred, sd = 1 / sqrt(n_pred))
  }
  if (is.null(design)) {
    design <- matrix(rnorm(n_obs * n_pred), nrow = n_obs, ncol = n_pred)
  }
  if (!is.null(intercept)) {
    if (!is.numeric(intercept)) {
      stop('The intercept argument must be numeric.')
    }
    coef_true <- c(intercept, coef_true)
    design <- cbind(rep(1, n_obs), design)
  }
  expected_mean <- as.vector(design %*% coef_true)
  noise_magnitude <- sqrt(var(expected_mean) / signal_to_noise^2)
  noise <- noise_magnitude * rnorm(n_obs)
  outcome <- expected_mean + noise
  return(list(design = design, outcome = outcome, coef_true = coef_true))
}

#' Function from HW2 to approximate the gradient of a given function via finite difference
approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  dimension <- length(x)
  numerical_grad <- rep(0, dimension)
  for(i in 1:dimension){
    e_i = replace(rep(0, dimension), i, 1)
    numerical_grad[i] = (func(x + dx*e_i) - func(x - dx*e_i))/(2*dx)
  }
  return(numerical_grad)
}
