## File Name: tam.jml.fit.R
## File Version: 9.204


tam.jml.fit <- function( tamobj, trim_val=10 )
{
    s1 <- Sys.time()
    resp <- tamobj$resp
    resp.ind <- tamobj$resp.ind
    A <- tamobj$A
    B <- tamobj$B
    nstud <- tamobj$nstud
    nitems <- tamobj$nitems
    maxK <- tamobj$maxK
    ItemScore <- tamobj$ItemScore
    theta <- tamobj$theta
    xsi <- tamobj$xsi
    pweightsM <- tamobj$pweights

    AXsi <- matrix(0, nrow=nitems, ncol=maxK)
    BB <- array(0, dim=c(nitems,maxK))
    B_Sq <- array(0,dim=c(nstud, nitems))
    C4 <- array(0,dim=c(nitems,nstud))
    for (k in 1:maxK){
        AXsi[,k] <- A[,k,] %*% xsi
    }
    B.0 <- B
    B.0[ is.na(B.0) ] <- 0
    B1 <- B.0[,,1]
    BB <- B1^2
    theta.unique <- unique(theta)
    NU <- length(theta.unique)
    B_bari <- array(0,dim=c(NU, nitems))
    BB_bari <- array(0, dim=c(NU, nitems))
    use_rcpp <- TRUE
    res <- tam_mml_calc_prob(iIndex=1:nitems, A=A, AXsi=AXsi,
                        B=B, xsi=xsi, theta=matrix( theta.unique, nrow=NU, ncol=1),
                        nnodes=NU, maxK=maxK, recalc=FALSE, use_rcpp=use_rcpp )
    rprobs <- res$rprobs
    rprobs[ is.na( rprobs) ] <- 0
    for (kk in 1:maxK){
        B_bari <- B_bari + t( B1[,kk]*rprobs[,kk,] )
        BB_bari <- BB_bari + t( BB[,kk] * rprobs[, kk, ] )
    }
    ind.theta <- match( theta, theta.unique)
    rprobs <- rprobs[,, ind.theta ]
    B_bari <- B_bari[ ind.theta, ]
    BB_bari <- BB_bari[ ind.theta, ]
    B_bari <- B_bari * resp.ind
    BB_bari <- BB_bari  * resp.ind
    B_var <- BB_bari - (B_bari^2)
    z_sq <- (resp - B_bari)^2/B_var
    zsd <- stats::sd(as.matrix(z_sq),na.rm=TRUE)
    z_sq[z_sq > trim_val*zsd] <-  trim_val*zsd   #Trim extreme values
    B_bariM <- aperm(outer(B_bari,rep(1,maxK)),c(2,3,1))
    B1M <- outer(B1,rep(1,nstud))
    for (kk in 1:maxK){
        C4 <- C4 + ( B1M[,kk,] - B_bariM[,kk,] )^4 * rprobs[,kk,]
    }
    C4 <- t(C4) * resp.ind
    #  outfitPerson <- apply(z_sq, 1, mean, na.rm=TRUE)
    outfitPerson <- rowMeans( z_sq, na.rm=TRUE )
    outfitItem <- colMeans(z_sq * pweightsM, na.rm=TRUE)

    infitPerson <- rowSums((resp - B_bari)^2, na.rm=TRUE) / rowSums(B_var, na.rm=TRUE)
    infitItem <- colSums((resp - B_bari)^2 * pweightsM, na.rm=TRUE) / colSums(B_var * pweightsM, na.rm=TRUE)

    z_sq.ind <- ! is.na(z_sq)
    var_outfit <- rowSums(C4/(B_var^2), na.rm=TRUE)/(rowSums(z_sq.ind)^2) - 1/rowSums(z_sq.ind)
    outfitPerson_t <- (outfitPerson^(1/3) - 1) * (3/sqrt(var_outfit)) + sqrt(var_outfit)/3

    var_outfit <- colSums(C4/(B_var^2), na.rm=TRUE)/(colSums(z_sq.ind)^2) - 1/colSums(z_sq.ind)
    outfitItem_t <- (outfitItem^(1/3) - 1) * (3/sqrt(var_outfit)) + sqrt(var_outfit)/3

    var_infit <- rowSums(C4-B_var^2, na.rm=TRUE)/((rowSums(B_var, na.rm=TRUE))^2)
    infitPerson_t <- (infitPerson^(1/3) - 1) * (3/sqrt(var_infit)) + sqrt(var_infit)/3

    var_infit <- colSums(C4-B_var^2, na.rm=TRUE)/((colSums(B_var, na.rm=TRUE))^2)
    infitItem_t <- (infitItem^(1/3) - 1) * (3/sqrt(var_infit)) + sqrt(var_infit)/3

    #--- collect item statistics
    fit.item <- data.frame(item=colnames(tamobj$resp), outfitItem=outfitItem,
                    outfitItem_t=outfitItem_t, infitItem=infitItem,
                    infitItem_t=infitItem_t)
    fit.person <- data.frame(outfitPerson=outfitPerson,
                    outfitPerson_t=outfitPerson_t, infitPerson=infitPerson,
                    infitPerson_t=infitPerson_t)

    #-- output
    s2 <- Sys.time()
    v1 <- c(s1, s2)
    res <- list(fit.item=fit.item, fit.person=fit.person, time=v1)
    return(res)
}
