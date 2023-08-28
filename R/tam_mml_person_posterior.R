## File Name: tam_mml_person_posterior.R
## File Version: 0.15

tam_mml_person_posterior <- function(pid, nstud, pweights,
    resp, resp.ind, snodes, hwtE, hwt, ndim, theta )
{
    # create a data frame person
    person <- data.frame( "pid"=pid, "case"=1:nstud, "pweight"=pweights )
    if ( ! is.null(resp) ){
        person$score <- rowSums( resp * resp.ind )
        person$max <- tam_mml_person_maxscore(resp=resp, resp.ind=resp.ind)
    }

    #-------------------
    # calculate EAP
    hwtE <- hwt
    if ( ndim==1 ){
        person$EAP <- tam_mml_person_EAP( hwt=hwtE, theta=theta[,1] )
        person$SD.EAP <- tam_mml_person_SD_EAP( hwt=hwtE, theta=theta[,1], EAP=person$EAP )
        EAP.rel <- tam_mml_person_EAP_rel(EAP=person$EAP, SD.EAP=person$SD.EAP,
                            pweights=pweights)
    } else {
        EAP.rel <- rep(0,ndim)
        names(EAP.rel) <- paste("Dim",1:ndim, sep="")
        for ( dd in 1:ndim ){
            person$EAP <- tam_mml_person_EAP( hwt=hwtE, theta=theta[,dd] )
            person$SD.EAP <- tam_mml_person_SD_EAP( hwt=hwtE, theta=theta[,dd], EAP=person$EAP )
            EAP.rel[dd] <- tam_mml_person_EAP_rel(EAP=person$EAP, SD.EAP=person$SD.EAP,
                                pweights=pweights)
            cnp <- colnames(person)
            colnames(person)[ which( cnp=="EAP" ) ] <- paste("EAP.Dim", dd, sep="")
            colnames(person)[ which( cnp=="SD.EAP" ) ] <- paste("SD.EAP.Dim", dd, sep="")
        }
    }

    #*** means and standard deviations of posterior distributions
    SD <- M <- rep(NA, ndim)
    post <- hwtE
    n <- nrow(post)
    for (dd in 1L:ndim){
        theta_dim <- theta[,dd]
        mt <- matrix( theta_dim, nrow=n, ncol=length(theta_dim), byrow=TRUE)
        M[dd] <- sum( mt*post*pweights ) / sum(pweights)
        M2 <- sum( mt^2*post*pweights ) / sum(pweights)
        SD[dd] <- sqrt( M2 - M[dd]^2 )
    }

    #----- OUTPUT
    res <- list( person=person, EAP.rel=EAP.rel, M_post=M, SD_post=SD )
    return(res)
}
