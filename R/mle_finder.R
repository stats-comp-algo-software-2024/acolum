#' Implement the MLE finder via pseudo-inverse (using Cholesky decomposition)
mle_pseudoinv <- function(design, outcome) {
  a <- crossprod(design)
  b <- crossprod(design, outcome)
  upper <- chol(a)
  z <- backsolve(upper, b, transpose = TRUE)
  beta <- as.vector(backsolve(upper, z))
  return(list(coef = beta))
}

#' Implement the MLE finder via BFGS for linear and logistic models
mle_BFGS <- function(design, outcome, model, ...) {
  init_coef <- rep(0, ncol(design))
  if (model == 'linear') {
    obj_fn <- function(coefs) {
      lm_log_likelihood(design, outcome, coefs, ...)
    }
    obj_grad <- function(coefs) {
      lm_loglik_gradient(design, outcome, coefs, ...)
    }
  }
  if (model == 'logit') {
    obj_fn <- function(coefs) {
      logit_log_likelihood(design, outcome, coefs)
    }
    obj_grad <- function(coefs) {
      logit_loglik_gradient(design, outcome, coefs)
    }
  }
  result <- stats::optim(
    par = init_coef, fn = obj_fn, gr = obj_grad,
    method = 'BFGS', control = list(fnscale = -1)
  )
  optim_converged <- (result$convergence == 0L)
  if (!optim_converged) {
    warning('Optimization did not converge. The estimates provided may be meaningless.')
  }
  return(list(coef = result$par, converged = optim_converged))
}
