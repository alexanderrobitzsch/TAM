## File Name: tam_np_inits.R
## File Version: 0.17


tam_np_inits <- function(dat, nodes, pweights, probs_init, n_basis, model="2PL")
{
    maxK <- apply(dat, 2, max, na.rm=TRUE)
    K <- max(maxK)
    K1 <- K + 1
    TP <- length(nodes)
    theta <- nodes
    I <- ncol(dat)
    items <- colnames(dat)

    #-- initial probabilities
    if ( is.null(probs_init) ){
        probs_cum <- probs <- array(0, dim=c(I,K+1,TP) )
        for (ii in 1:I){
            K_ii <- maxK[ii]
            K_ii2 <- K_ii+2
            q1 <- stats::qlogis( seq( 0,1, len=K_ii2)[ -c(1,K_ii2) ] )
            theta1 <- tam_matrix2(theta, nrow=1, ncol=TP)
            for (kk in 1:K_ii){
                probs_cum[ii,kk,] <- stats::plogis(q1[kk] - theta1 )
            }
            probs_cum[ ii, K_ii+1,] <- 1
            probs[ii,1,] <- probs_cum[ii,1,]
            for (kk in 2:(K_ii+1) ){
                probs[ii,kk,] <- probs_cum[ii,kk,] - probs_cum[ii,kk-1, ]
            }
        }
    } else {
        probs <- probs_init
    }
    #--- initial theta probabilities
    pi.k <- stats::dnorm(theta, mean=0, sd=1)
    pi.k <- pi.k / sum(pi.k)
    #--- data matrices
    dat_resp <- ! is.na(dat)
    dat2 <- dat
    dat2[ ! dat_resp ] <- 0
    dat2 <- as.matrix(dat2)
    N <- nrow(dat)
    if (is.null(pweights)){
        pweights <- rep(1,N)
    }
    # normalize weights
    pweights <- N * pweights / sum(pweights)
    # use_basis
    use_basis <- ( n_basis > 0 ) | ( model=="1PL")

    #-- output
    res <- list(maxK=maxK, K=K, K1=K1, TP=TP, I=I, probs=probs, pi.k=pi.k,
                dat_resp=dat_resp, dat2=dat2, N=N, pweights=pweights, theta=theta,
                items=items, use_basis=use_basis)
    return(res)
}
