## File Name: tamaan.3pl.lca.R
## File Version: 9.11

#######################################################################
# tamaan 3PL LCA module
tamaan.3pl.lca <- function( res0, anal.list, con, ... )
{
    if ( ! is.null( anal.list$NSTARTS ) ){
        NSTARTS <- anal.list$NSTARTS
    } else {
        NSTARTS <- c(0,0)
    }

    #*** initial gammaslope estimate
    # different starts if NSTARTS > 0
    con0 <- con
    con0$maxiter <- NSTARTS[2]
    con0$progress <- FALSE
    devmin <- 1E100
    Edes <- NULL

    if (NSTARTS[1] > 0 ){
        for (nn in 1:(NSTARTS[1]) ){
            gammaslope <- c( stats::qlogis( stats::runif( dim(res0$E)[4] - 1 ) ), 1 )
            TP <- dim(res0$E)[3]
            # delta.inits
            if (nn==1){ delta.inits <- NULL }
            if (nn>1){
                G <- ncol(res$delta)
                delta.inits <- inits.delta.lca( G, TP, nn  )
            }
            res <- tam.mml.3pl(resp=res0$resp, E=res0$E, skillspace="discrete",
                        theta.k=res0$theta.k, gammaslope=gammaslope,
                        notA=res0$notA, control=con0, delta.inits=delta.inits,
                        gammaslope.fixed=res0$gammaslope.fixed,
                        Edes=Edes, ... )
            Edes <- res$Edes
            if (con$progress){
                cat( paste0( "*** Random Start ", nn,
                        " | Deviance=", round( res$deviance, 2 ), "\n") )
                utils::flush.console()
            }
            if ( res$deviance < devmin ){
                devmin <- res$deviance
                gammaslope.min <- res$gammaslope
                delta.min <- res$delta
            }
        }
    }
    # use inits or best solution from random starts
    if (NSTARTS[1] > 0 ){
        gammaslope <- gammaslope.min
        delta.inits <- delta.min
    } else {
        gammaslope <- c( stats::qlogis( stats::runif( dim(res0$E)[4] - 1 ) ), 1 )
        delta.inits <- NULL
    }

    res <- tam.mml.3pl(resp=res0$resp, E=res0$E, skillspace="discrete",
                        theta.k=res0$theta.k, gammaslope=gammaslope,
                        gammaslope.fixed=res0$gammaslope.fixed,
                        notA=res0$notA, delta.inits=delta.inits,  control=con,
                        Edes=Edes, ... )
    #--- LCA probabilities
    res0 <- tamaan_3pl_lca_extract_lcaprobs(res=res)
    res$lcaprobs <- res0$lcaprobs
    res$lca_M <- res0$lca_M

    res$tamaan.method <- "tam.mml.3pl"
    return(res)
}
#######################################################################

######################################################
# inits delta parameters
inits.delta.lca <- function( G, TP, nn  )
{
    delta.inits <- matrix( 1/TP, nrow=TP, ncol=G )
    delta.inits <- delta.inits + nn/8 * stats::runif(TP*G)
    delta.inits <- delta.inits / colSums(delta.inits)
    delta.inits <- log(delta.inits)
    return(delta.inits)
}
########################################################
