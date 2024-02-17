#' @export
hiper_glm <- function(design, outcome, model = 'linear', method = 'BFGS') {
    supported_models <- c('linear', 'logit')
    if (!(model %in% supported_models)){
      stop(sprintf('This model (%s) is not supported.', model))
    }
    if (model == 'linear') {
      if (method == 'BFGS') {
        hglm_out <- mle_BFGS(design, outcome, model)
      } else if (method == 'pseudoinv') {
        hglm_out <- mle_pseudoinv(design, outcome)
      } else {
        stop(sprintf('This method (%s) is not supported; please use \'BFGS\' or \'pseudoinv\' for linear regression.', method))
      }
    }
    if (model == 'logit') {
      if (method == 'BFGS') {
        hglm_out <- mle_BFGS(design, outcome, model)
      } else if (method == 'newton') {
        hglm_out <- mle_newton(design, outcome, tol = 1e-8)
      } else {
        stop(sprintf('This method (%s) is not supported; please use \'BFGS\' or \'newton\' for logistic regression.', method))
      }
    }
    class(hglm_out) <- 'hglm'
    return(hglm_out)
}
