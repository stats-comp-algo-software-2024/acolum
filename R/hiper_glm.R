#' @export
hiper_glm <- function(design, outcome, model = 'linear') {
    # Define supported models
    supported_models <- c('linear', 'logit')
    # Check for supported models
    if (!(model %in% supported_models)){
      stop(sprintf('This model (%s) is not supported.', model))
    }
    # Warn that the function is yet to be implemented
    warning('This function is yet to be implemented.')
    # TODO: Implement model fitting by MLE
    hglm_out <- list()
    class(hglm_out) <- 'hglm'
    return(hglm_out)
}
