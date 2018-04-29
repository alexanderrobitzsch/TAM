//// File Name: tam_rcpp_mml_proc.cpp
//// File Version: 0.06


#include <Rcpp.h>

using namespace Rcpp;

///********************************************************************
///** tam_rcpp_mml_maxcat
// [[Rcpp::export]]
Rcpp::IntegerVector tam_rcpp_mml_maxcat( Rcpp::NumericVector A, Rcpp::IntegerVector dimA )
{
    int I = dimA[0];
    int K = dimA[1];
    Rcpp::IntegerVector maxcat(I);
    for (int ii=0; ii<I; ii++){
        for (int hh=1; hh<K; hh++){
            if ( ! R_IsNA( A[ ii + hh*I ] ) ){
                maxcat[ii] = hh + 1;
            }
        }
    }
    //-- output
    return maxcat;
}
///********************************************************************
