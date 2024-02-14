#' @export
hiper_glm <- function(design, outcome, model = 'linear', method = 'BFGS') {
    supported_model <- c('linear')
    if (!(model %in% supported_model)){
      stop(sprintf('This model (%s) is not supported.', model))
    }
    hglm_out <- list()
    class(hglm_out) <- 'hglm'
    if (model == 'linear') {
      if (method == 'pseudoinv') {
        hglm_out$coef <- lm_mle_pseudoinv(design, outcome)
      } else if (method == 'BFGS') {
        hglm_out$coef <- lm_mle_BFGS(design, outcome)
      } else {
        stop('The function is yet to be implemented for methods other than pseudo-inverse or BFGS.')
      }
    }
    return(hglm_out)
}
