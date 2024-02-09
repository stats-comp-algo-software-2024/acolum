#' @export
hiper_glm <- function(design, outcome, model = 'linear', method = 'BFGS') {
    # Define supported model
    supported_model <- c('linear')
    # Check if input is not the supported model
    if (!(model %in% supported_model)){
      stop(sprintf('This model (%s) is not supported.', model))
    }
    # Implement the MLE finder via pseudo-inverse and BFGS
    if (model == 'linear') {
      hglm_out <- list()
      class(hglm_out) <- 'hglm'
      if (method == 'pseudoinv') {
        hglm_out$coef <- glm_mle_pseudoinv(design, outcome)
      } else if (method == 'BFGS') {
        hglm_out$coef <- glm_mle_BFGS(design, outcome)
      } else {
        stop("The function is yet to be implemented for methods other than pseudo-inverse or BFGS.")
      }
    }
    return(hglm_out)
}
