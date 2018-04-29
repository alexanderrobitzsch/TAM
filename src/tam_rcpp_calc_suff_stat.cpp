//// File Name: tam_rcpp_calc_suff_stat.cpp
//// File Version: 0.13

// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// using namespace arma;

///********************************************************************
///** tam_rcpp_calc_suff_stat
// [[Rcpp::export]]
Rcpp::List tam_rcpp_calc_suff_stat( Rcpp::IntegerMatrix resp, Rcpp::IntegerMatrix resp_ind,
        int maxK, int nitems, Rcpp::NumericVector pweights, Rcpp::NumericMatrix cA )
{
    int N = resp.nrow();
    int I = nitems;
    int ngr = I*maxK;
    Rcpp::IntegerMatrix cResp(N,ngr);
    Rcpp::NumericVector colsums_cResp(ngr);
    // col.index <- rep( 1:nitems , each = maxK )
    // cResp <- (resp +1) *resp.ind
    // cResp <- cResp[ , col.index  ]
    // cResp <- 1 * ( cResp == matrix( rep(1:(maxK), nitems) , nrow(cResp) , ncol(cResp) , byrow=TRUE ) )
    int temp_index = 0;
    for (int ii=0; ii<I; ii++){
        for (int nn=0; nn<N; nn++){
            if ( resp_ind(nn,ii) == 1 ){
                temp_index = resp(nn,ii) + ii*maxK;
                cResp(nn, temp_index) = 1;
                colsums_cResp[ temp_index ] += pweights[nn];
            }
        }
    }
    // if ( stats::sd(pweights) > 0 ){
    //    ItemScore <- as.vector( t( colSums( cResp * pweights ) ) %*% cA )
    //}  else {
    //    ItemScore <- as.vector( t( colSums( cResp) ) %*% cA )
    //}
    int NP = cA.ncol();
    Rcpp::NumericVector ItemScore(NP);
    for (int pp=0; pp<NP; pp++){
        for (int hh=0; hh<ngr; hh++){
            if (cA(hh,pp) != 0){
                ItemScore[pp] += colsums_cResp[hh] * cA(hh,pp);
            }
        }
    }
    /// OUTPUT
    return Rcpp::List::create(
                Rcpp::Named("cResp") = cResp,
                Rcpp::Named("ItemScore") = ItemScore
            );
}
///********************************************************************

