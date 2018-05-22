## File Name: tam_calc_exp.R
## File Version: 9.18


###########################################################
tam_calc_exp <- function( rprobs, A, np, est.xsi.index, itemwt,
    indexIP.no, indexIP.list2, Avector )
{
    CC <- dim(rprobs)[2]
    TP <- dim(rprobs)[3]
    NXSI <- dim(A)[3]
    NI <- dim(A)[1]
    rprobsL1 <- as.vector( rprobs )
    AL1 <- Avector
    rprobsL1 <- tam_rcpp_calc_exp_redefine_vector_na( A=rprobsL1, val=0 )
    res <- tam_rcpp_calc_exp( NP=np, rprobs=rprobsL1, A=AL1, INDEXIPNO=indexIP.no,
            INDEXIPLIST2=indexIP.list2, ESTXSIINDEX=est.xsi.index, C=CC,
            ITEMWT=itemwt, NR=NI*CC, TP=TP )
    return(res)
}

calc_exp_TK3 <- tam_calc_exp

