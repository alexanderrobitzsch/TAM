//// File Name: tam_rcpp_calc_prob.cpp
//// File Version: 1.605

// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// using namespace arma;

///********************************************************************
///** tam_rcpp_calc_prob_subtract_max
// [[Rcpp::export]]
Rcpp::NumericMatrix tam_rcpp_calc_prob_subtract_max( Rcpp::NumericMatrix rr0M,
        int NI , int NK , int TP)
{
    int NR = rr0M.nrow() ;
    Rcpp::NumericMatrix rr1M( NR, TP );
    double val=0;
    double Mval=0;
    int ind1=0;
    //*** loop over items and categories
    for (int ii=0; ii<NI; ii++){
        for (int tt=0; tt<TP; tt++){
            Mval = rr0M( ii , tt ) ;
            for (int hh=1;hh<NK;hh++){
                val = rr0M( hh*NI + ii, tt ) ;
                if ( ! R_IsNA( val ) ){
                    if ( val > Mval ){
                        Mval = val ;
                    }
                }
            }
            for (int hh=0; hh<NK; hh++){
                ind1 = hh*NI + ii ;
                val = rr0M( ind1 , tt ) ;
                if ( ! R_IsNA( val ) ){
                    rr1M( ind1 , tt ) = rr0M( ind1 , tt ) - Mval ;
                } else {
                    rr1M( ind1 , tt ) = NA_REAL ;
                }
            }
        }
    }
    /// OUTPUT
    return rr1M ;
}
///********************************************************************

///********************************************************************
///** tam_rcpp_calc_prob
// [[Rcpp::export]]
Rcpp::List tam_rcpp_calc_prob( Rcpp::NumericVector A, Rcpp::IntegerVector dimA,
            Rcpp::NumericVector xsi, Rcpp::IntegerVector maxcat, Rcpp::NumericMatrix AXsi0,
            Rcpp::IntegerVector iIndex, Rcpp::NumericMatrix theta, Rcpp::NumericVector B )
{
    int I = dimA[0];
    int maxK = dimA[1];
    int NP = dimA[2];
    int D = theta.ncol();
    int TP = theta.nrow();
    Rcpp::NumericMatrix AXsi(I,maxK);
    int LI = iIndex.size();
    Rcpp::NumericMatrix AXsi_tmp(LI,maxK);
    Rcpp::NumericVector rprobs(LI*maxK*TP);
    int item_ii=0;
    double val=0;

    //**** compute AXsi
    for (int kk=0; kk<maxK; kk++){
        AXsi(_,kk) = AXsi0(_,kk);
    }
    for (int ii=0; ii<LI; ii++){
        item_ii = iIndex[ii] - 1;
        for (int hh=0; hh<maxcat[item_ii]; hh++){
            for (int pp=0; pp<NP; pp++){
                val = A[ item_ii + hh*I + pp*I*maxK ];
                if (val != 0){
                    AXsi_tmp(ii,hh) += val * xsi[pp];
                }
            }
            AXsi( item_ii, hh ) = AXsi_tmp(ii,hh);
        }
    }

    //**** compute item response probabilities
    Rcpp::NumericVector prob_temp(maxK);
    double sumtemp=0;
    double Bval=0;
    for (int ii=0; ii<LI; ii++){
        item_ii = iIndex[ii] - 1;
        for (int tt=0; tt<TP; tt++){
            for (int hh=0; hh<maxcat[item_ii]; hh++){
                prob_temp[hh] = AXsi_tmp(ii,hh);
                for (int dd=0; dd<D; dd++){
                    Bval = B[ item_ii + hh*I + dd*I*maxK ];
                    if (Bval != 0){
                        prob_temp[hh] += Bval * theta(tt,dd);
                    }
                }
                if (hh>0){
                    if (prob_temp[hh] > val ){
                        val = prob_temp[hh];
                    }
                } else {
                    val = prob_temp[hh];
                }
            }
            sumtemp=0;
            for (int hh=0; hh<maxcat[item_ii]; hh++){
                prob_temp[hh] = std::exp( prob_temp[hh] - val );
                sumtemp += prob_temp[hh];
            }
            for (int hh=0; hh<maxcat[item_ii]; hh++){
                rprobs[ ii+hh*LI+tt*LI*maxK ] = prob_temp[hh] / sumtemp;
            }
        }
    }
    ///-- OUTPUT
    return Rcpp::List::create(
            Rcpp::Named("AXsi")= AXsi,
            Rcpp::Named("rprobs")= rprobs
            );
}
///********************************************************************
