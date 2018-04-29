## File Name: logLik_tam.R
## File Version: 9.04


###############################################################
# log-likelihood function tam.mml
logLik.tam <- function (object, ...)
{
    # extract log-likelihood
    out <- - object$ic$deviance / 2
    # number of parameters
    attr(out, "df") <- object$ic$Npars
    # extract number of observations
    attr(out, "nobs") <- object$ic$n
    class(out) <- "logLik"
    return(out)
}
logLik.tam.mml <- logLik.tam
################################################################

logLik.tamaan <- logLik.tam.mml.3pl <- logLik.tam
logLik.tam.latreg <- logLik.tam
