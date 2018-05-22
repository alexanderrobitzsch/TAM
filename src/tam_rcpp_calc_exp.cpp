//// File Name: tam_rcpp_calc_exp.cpp
//// File Version: 3.50


#include <Rcpp.h>

using namespace Rcpp;




///********************************************************************
///** tam_rcpp_calc_exp
// [[Rcpp::export]]
Rcpp::List tam_rcpp_calc_exp( int NP, Rcpp::NumericVector rprobs,
    Rcpp::NumericVector A, Rcpp::NumericMatrix INDEXIPNO,
    Rcpp::NumericVector INDEXIPLIST2, Rcpp::NumericVector ESTXSIINDEX,
    int C, Rcpp::NumericMatrix ITEMWT, int NR, int TP)
{
    ////////////////////////////////////////////////////////////
    // define output vectors
    Rcpp::NumericVector XBAR (NP);
    Rcpp::NumericVector XBAR2 (NP);
    Rcpp::NumericVector XXF (NP);

    ///////////////////////////////////////////////////////////
    // DEFINE indices
    int NEXI = ESTXSIINDEX.size();
    int II = NR / C;

    /////////////////////////////////////////////////////////
    // CALCULATIONS

    int pp=0;
    int ii=0;
    int GG=0;
    double ll = 0; // xbar element
    double yy = 0; // xbar2 element
    double zz = 0; // xxf element
    double vv1 = 0;  // xbar counter
    double vv2 = 0;  // xxf counter
    double temp1=0;
    double temp2=0;

    // loop over xsi item parameters
    for (int hh=0;hh<NEXI;++hh){
        pp = ESTXSIINDEX[hh] - 1;
        ll = 0; // xbar element
        yy = 0; // xbar2 element
        zz = 0; // xxf element

        // loop over theta points
        for (int tt=0;tt<TP;++tt){
            // loop over items within an item parameter group
            GG = INDEXIPNO(pp,1) - INDEXIPNO(pp,0) + 1;
            for (int gg=0;gg<GG;++gg){
                ii=INDEXIPLIST2[ INDEXIPNO(pp,0)-1 + gg ]-1;
                // loop over categories cc = 1, ..., C
                vv1 = 0;  // xbar counter
                vv2 = 0;  // xxf counter
                for (int cc=0;cc<C;++cc){
                    // xbar calculation
                    temp2 = A[ ii+cc*II + pp*II*C ] * rprobs[ ii + cc*II + tt*II*C ];
                    vv1 += temp2;
                    vv2 += A[ ii+cc*II + pp*II*C] * temp2;
                }
                temp1 = vv1*ITEMWT(tt,ii);
                ll += temp1; // xbar addition
                yy += vv1*temp1; // xbar2 addition
                zz += vv2*ITEMWT(tt,ii); // xxf addition
            }
        }
        XBAR[pp] = ll;
        XBAR2[pp] = yy;
        XXF[pp] = zz;
    } // end xsi index

    ///////////////////////////////////////////////////////
    ///////////// O U T P U T   ///////////////////////////
    return Rcpp::List::create(
            Rcpp::Named("xbar")=XBAR,
            Rcpp::Named("xbar2")=XBAR2,
            Rcpp::Named("xxf")=XXF
        );
}


///********************************************************************
///** tam_rcpp_calc_exp_redefine_vector_na
// [[Rcpp::export]]
Rcpp::NumericVector tam_rcpp_calc_exp_redefine_vector_na( Rcpp::NumericVector A, double val )
{
    int N = A.size();
    Rcpp::NumericVector A1(N);
    for( int nn=0;nn<N;nn++){
        if ( R_IsNA( A[nn] ) ){
            A1[nn] = val;
        } else {
            A1[nn] = A[nn];
        }
    }
    //---- OUTPUT
    return A1;
}



