## File Name: tam_mml_3pl_calc_exp.R
## File Version: 1.11



###########################################################
# faster version of calc_exp_TP3
tam_mml_3pl_calc_exp <- function( rprobs, A, np, est.xsi.index, itemwt,
    indexIP.no, indexIP.list2, rprobs0, guess, n.ik, N.ik)
{
    CC <- dim(rprobs)[2]
    TP <- dim(rprobs)[3]
    NXSI <- dim(A)[3]
    NI <- dim(A)[1]
    # restructure rprobs and AL
    AL <- matrix( A, nrow=NI*CC, ncol=NXSI )
    AL[ is.na(AL) ] <- 0
    rprobsL <- matrix( rprobs, nrow=NI*CC, ncol=TP )
    rprobsL[ is.na(rprobsL) ] <- 0
    rprobsL0 <- matrix( rprobs0, nrow=NI*CC, ncol=TP )
    rprobsL0[ is.na(rprobsL0) ] <- 0
    # use nik
    nik <- as.vector(n.ik)
    ni <- as.vector(N.ik)
    # rcpp call
    res <- tam_rcpp_mml_3pl_calcexp(  np, rprobsL, AL,    indexIP.no,
                indexIP.list2, est.xsi.index, CC, itemwt, rprobsL0,
                guess, nik, ni )
    return(res)
}

.mml.3pl.calc_exp_TK3 <- tam_mml_3pl_calc_exp
