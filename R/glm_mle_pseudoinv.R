#' Implement the MLE finder via pseudo-inverse (using Cholesky decomposition)
glm_mle_pseudoinv <- function(design, outcome) {
  a <- crossprod(design)
  b <- crossprod(design, outcome)
  upper <- chol(a)
  z <- backsolve(upper, b, transpose = TRUE)
  beta <- as.vector(backsolve(upper, z))
  return(beta)
}
