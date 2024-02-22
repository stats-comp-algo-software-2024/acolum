#' Check if two quantities are close enough to each other
are_all_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  are_all_within_atol <- all(abs_diff < abs_tol)
  are_all_within_rtol <- all(abs_diff < rel_tol * pmax(abs(v), abs(w)))
  return(are_all_within_atol && are_all_within_rtol)
}

#' Calculate the log-likelihood for a linear model
lm_log_likelihood <- function(design, outcome, beta, noise_var = 1) {
  est <- design %*% beta
  loglik <- -0.5 / noise_var * sum((outcome - est)^2)
  return(loglik)
}

#' Calculate the gradient for a linear model
lm_loglik_gradient <- function(design, outcome, beta, noise_var = 1) {
  est <- design %*% beta
  grad <- t(design) %*% (outcome - est) / noise_var
  return(grad)
}

#' Calculate the sigmoid function for a logistic regression model
sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

#' Calculate the log-likelihood for a logistic regression model
logit_log_likelihood <- function(design, outcome, beta) {
  est <- design %*% beta
  loglik <- sum(outcome * est - log(1 + exp(est)))
  return(loglik)
}

#' Calculate the gradient for a logistic regression model
logit_loglik_gradient <- function(design, outcome, beta) {
  est <- design %*% beta
  mu <- sigmoid(est)
  grad <- as.vector(t(design) %*% (outcome - mu))
  return(grad)
}

#' Calculate the Hessian of the log-likelihood for a logistic regression model
logit_hessian <- function(design, beta) {
  est <- design %*% beta
  mu <- as.vector(sigmoid(est))
  weight <- diag(mu * (1 - mu))
  hess <- -t(design) %*% weight %*% design
  return(hess)
}
