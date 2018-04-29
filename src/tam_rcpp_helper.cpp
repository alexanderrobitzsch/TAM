//// File Name: tam_rcpp_helper.cpp
//// File Version: 3.20

// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// using namespace arma;




///********************************************************************
///** tam_rcpp_theta_sq
// [[Rcpp::export]]
Rcpp::NumericMatrix tam_rcpp_theta_sq( Rcpp::NumericMatrix theta )
{
    int N = theta.nrow() ;
    int D = theta.ncol() ;
    int D2 = D*D;
    Rcpp::NumericMatrix thetasq(N,D2) ;

    //----- calculation of squared theta matrix
    for (int nn=0;nn<N;nn++){
        for (int dd1=0;dd1<D;dd1++){
            thetasq(nn, dd1*D + dd1 ) = pow( theta(nn,dd1) , 2 ) ;
            for (int dd2=dd1+1;dd2<D;dd2++){
                thetasq(nn,dd1*D + dd2 ) = theta(nn,dd1) * theta(nn,dd2 ) ;
                thetasq(nn,dd2*D + dd1 ) = thetasq(nn,dd1*D + dd2) ;
            }
        }
    }
    //// OUTPUT
    return thetasq ;
}

///********************************************************************
///** tam_rcpp_interval_index
// [[Rcpp::export]]
Rcpp::NumericVector tam_rcpp_interval_index( Rcpp::NumericMatrix MATR,
    Rcpp::NumericVector RN )
{
    int NR=MATR.nrow();
    int NC=MATR.ncol();
    //-- create output vectors
    Rcpp::NumericVector IND(NR);
    IND.fill(0);
    for (int nn=0;nn<NR;++nn){
        for (int cc=0 ; cc < NC ; ++cc ){
            if ( MATR(nn,cc) > RN[nn] ){
                IND(nn) = cc + 1 ;
                break ;
            }
        }
    }
    ///-- OUTPUT
    return IND;
}


