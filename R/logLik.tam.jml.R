## File Name: logLik.tam.jml.R
## File Version: 0.04

###############################################################
# log-likelihood function tam.jml
logLik.tam.jml <- function (object, ...)
{
    # extract log-likelihood
    out <- - object$deviance / 2
    #*** calculate number of parameters
    nstud <- object$nstud
    npar <- length(object$xsi) + nstud - 1
    if ( ! is.null(object$xsi.fixed) ){
        npar <- npar - nrow(object$xsi.fixed) + 1
    }
    # number of parameters
    attr(out, "df") <- npar
    # extract number of observations
    attr(out, "nobs") <- nstud
    class(out) <- "logLik"
    return(out)
}
