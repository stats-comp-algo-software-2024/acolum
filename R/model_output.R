#' @export
coef.hglm <- function(hglm_out) {
  coef <- hglm_out$coef
  return(coef)
}

#' @export
vcov.hglm <- function(hglm_out) {
  warning('This method is yet to be implemented.')
  # TODO: Implement vcov.hglm S3 method
  cat('The variance-covariance matrix is ...' )
}

#' @export
print.hglm <- function(hglm_out) {
  warning('This method is yet to be implemented.')
  # TODO: Implement print.hglm S3 method
  cat('The model is ...')
}
