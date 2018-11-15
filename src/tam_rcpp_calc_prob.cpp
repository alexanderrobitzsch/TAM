//// File Name: tam_rcpp_calc_prob.cpp
//// File Version: 1.627

// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// using namespace arma;

///********************************************************************
///** tam_rcpp_calc_prob_subtract_max
// [[Rcpp::export]]
Rcpp::NumericMatrix tam_rcpp_calc_prob_subtract_max( Rcpp::NumericMatrix rr0M,
        int NI, int NK, int TP)
{
    int NR = rr0M.nrow();
    Rcpp::NumericMatrix rr1M( NR, TP );
    double val=0;
    double Mval=0;
    int ind1=0;
    //*** loop over items and categories
    for (int ii=0; ii<NI; ii++){
        for (int tt=0; tt<TP; tt++){
            Mval = rr0M( ii, tt );
            for (int hh=1;hh<NK;hh++){
                val = rr0M( hh*NI + ii, tt );
                if ( ! R_IsNA( val ) ){
                    if ( val > Mval ){
                        Mval = val;
                    }
                }
            }
            for (int hh=0; hh<NK; hh++){
                ind1 = hh*NI + ii;
                val = rr0M( ind1, tt );
                if ( ! R_IsNA( val ) ){
                    rr1M( ind1, tt ) = rr0M( ind1, tt ) - Mval;
                } else {
                    rr1M( ind1, tt ) = NA_REAL;
                }
            }
        }
    }
    /// OUTPUT
    return rr1M;
}
///********************************************************************

///********************************************************************
///** tam_rcpp_calc_prob_subtract_max_exp
// [[Rcpp::export]]
Rcpp::NumericVector tam_rcpp_calc_prob_subtract_max_exp( Rcpp::NumericVector rr0,
        Rcpp::IntegerVector dim_rr )
{

    int NI = dim_rr[0];
    int NK = dim_rr[1];
    int TP = dim_rr[2];
    int NR = NI*NK*TP;
    Rcpp::NumericVector rr1(NR);
    double val=0;
    double Mval=0;
    int ind1=0;
    //*** loop over items and categories
    for (int ii=0; ii<NI; ii++){
        for (int tt=0; tt<TP; tt++){
            Mval = rr0[ ii + tt*NI*NK ];
            for (int hh=1; hh<NK; hh++){
                val = rr0[ ii + hh*NI + tt*NI*NK ];
                if ( ! R_IsNA(val) ){
                    if ( val > Mval ){
                        Mval = val;
                    }
                }
            }
            for (int hh=0; hh<NK; hh++){
                ind1 = ii + hh*NI + tt*NI*NK;
                val = rr0[ ind1 ];
                if ( ! R_IsNA( val ) ){
                    rr1[ind1] = rr0[ ind1 ] - Mval;
                    rr1[ind1] = std::exp( rr1[ind1] );
                } else {
                    rr1[ind1] = NA_REAL;
                }
            }
        }
    }
    /// OUTPUT
    return rr1;
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


///********************************************************************
///** tam_rcpp_tam_mml_calc_prob_R_outer_Btheta
// [[Rcpp::export]]
Rcpp::NumericVector tam_rcpp_tam_mml_calc_prob_R_outer_Btheta(
        Rcpp::NumericVector Btheta, Rcpp::NumericVector B_dd,
        Rcpp::NumericVector theta_dd, Rcpp::IntegerVector dim_Btheta )
{
    // dim_Btheta = c(length(iIndex), maxK, nnodes)
    int LI = dim_Btheta[0];
    int maxK = dim_Btheta[1];
    int nnodes = dim_Btheta[2];
    int NM = LI*maxK*nnodes;
    Rcpp::NumericVector Btheta_add(NM);
    Btheta_add.fill(0);
    int ind_hh = 0;

    for (int ii=0; ii<LI; ii++){
        for (int cc=0; cc<maxK; cc++){
            ind_hh = ii+cc*LI;
            if (B_dd[ind_hh] != 0){
                for (int tt=0; tt<nnodes; tt++){
                    Btheta_add[ii+cc*LI+tt*LI*maxK] = B_dd[ind_hh]*theta_dd[tt];
                }
            }
        }
    }

    //--- output
    return Btheta_add;
}
///********************************************************************



///********************************************************************
///** tam_rcpp_tam_mml_calc_prob_R_normalize_rprobs
// [[Rcpp::export]]
Rcpp::NumericVector tam_rcpp_tam_mml_calc_prob_R_normalize_rprobs(
        Rcpp::NumericVector rr, Rcpp::IntegerVector dim_rr)
{
    int I = dim_rr[0];
    int K = dim_rr[1];
    int TP = dim_rr[2];
    int NM = I*K*TP;
    Rcpp::NumericVector rprobs(NM);
    double tempval=0;
    int ind=0;
    for (int ii=0; ii<I; ii++){
        for (int tt=0; tt<TP; tt++){
            tempval=0;
            for (int cc=0; cc<K; cc++){
                ind = ii+cc*I+tt*I*K;
                rprobs[ind] = rr[ind];
                if ( ! R_IsNA( rr[ind] )){
                    tempval += rr[ind];
                }
            }
            for (int cc=0; cc<K; cc++){
                if ( ! R_IsNA( rr[ii+cc*I+tt*I*K] )){
                    ind = ii+cc*I+tt*I*K;
                    rprobs[ind] = rr[ind] / tempval;
                }
            }
        }
    }
    //--- output
    return rprobs;
}
///********************************************************************
