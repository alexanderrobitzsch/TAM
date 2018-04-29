//// File Name: tam_rcpp_mml_2pl.cpp
//// File Version: 0.513


#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** tam_rcpp_mml_2pl_mstep_item_slopes_suffstat
// [[Rcpp::export]]
Rcpp::List tam_rcpp_mml_2pl_mstep_item_slopes_suffstat( Rcpp::NumericVector rprobs,
        Rcpp::IntegerVector items_temp, Rcpp::NumericMatrix theta, int dd, int LIT, int TP,
        int nitems, Rcpp::IntegerVector maxcat, int maxK, Rcpp::NumericMatrix itemwt,
        Rcpp::NumericMatrix xxf_, Rcpp::NumericMatrix xbar_, Rcpp::NumericMatrix xbar2_,
        Rcpp::CharacterVector irtmodel, Rcpp::NumericMatrix xtemp_,
        Rcpp::IntegerVector items_conv)
{
    // rprobs.k <- matrix( rprobs[,k,] , nrow=LIT , ncol=TP )
    // rpr.it <- t( rprobs.k ) * itemwt[,items.temp]
    // int I=nitems;
    // int kk=0;
    int item_ii=0;
    int NC = items_conv.size();

    // ttheta.dd2 <- t( theta[,dd,drop=FALSE]^2)
    // ttheta.dd <- t( theta[,dd,drop=FALSE] )
    // xbar[items.temp,k] <- ttheta.dd %*% rpr.it
    // xxf[items.temp,k] <- ttheta.dd2 %*% rpr.it

    Rcpp::NumericMatrix rpr_it(TP,LIT);
    Rcpp::NumericMatrix xbar(nitems, maxK);
    Rcpp::NumericMatrix xbar2(nitems, maxK);
    Rcpp::NumericMatrix xxf(nitems, maxK);
    int NT = xtemp_.ncol();
    Rcpp::NumericMatrix xtemp( xtemp_.nrow(), NT );
    double temp1=0;
    double temp2=0;
    double probs_temp=0;
    if ( ( irtmodel[0] == "GPCM" ) | ( irtmodel[0] == "GPCM.design" ) ){
        for (int tt=0; tt<NT; tt++){
            xtemp(_,tt) = xtemp_(_,tt);
        }
    }
    double kk2=0.0;
    int hh1=0;

    for (int kk=0; kk<maxK; kk++){
        xbar(_,kk) = xbar_(_,kk);
        xxf(_,kk) = xxf_(_,kk);
        xbar2(_,kk) = xbar2_(_,kk);
        for (int hh=0; hh<NC; hh++){
            hh1 = items_conv[hh] - 1;
            if (hh1>=0){
                xbar(hh1,kk) = 0;
                xxf(hh1,kk) = 0;
                xbar2(hh1,kk) = 0;
            }
        }
        kk2 = std::pow( kk - 0.0, 2.0);
        rpr_it.fill(0);
        for (int ii=0; ii<LIT; ii++){
            item_ii = items_temp[ii] - 1;
            xbar(item_ii,kk) = 0;
            xxf(item_ii,kk) = 0;
            xbar2(item_ii,kk) = 0;
            if ( kk < maxcat[item_ii] ){
                for (int tt=0; tt<TP; tt++){
                    probs_temp = rprobs[ ii+kk*LIT+tt*LIT*maxK ];
                    rpr_it(tt,ii) = probs_temp*itemwt(tt,item_ii);
                    temp1 = theta(tt,dd)*rpr_it(tt,ii); // ttheta.dd %*% rpr.it
                    xbar(item_ii,kk) += temp1;
                    temp2 = temp1*theta(tt,dd);  // ttheta.dd2 %*% rpr.it
                    xxf(item_ii,kk) += temp2;
                    //----- 2PL model --------
                    if ( irtmodel[0] == "2PL" ){
                        //    xbar2[items.temp,k] <- ttheta.dd2 %*% ( t( rprobs.k^2 ) * itemwt[,items.temp] )
                        xbar2(item_ii,kk) += probs_temp * temp2;
                    }
                    //----- GPCM --------
                    if ( ( irtmodel[0] == "GPCM" ) | ( irtmodel[0] == "GPCM.design" ) ){
                        // xtemp <- xtemp + tam_matrix2(theta[,dd],nrow=LIT,ncol=TP) * rprobs[,k,] * ( k-1 )
                        xtemp( item_ii, tt) += theta(tt,dd) * probs_temp * kk;
                    }
                }
                //----- GPCM --------
                if ( ( irtmodel[0] == "GPCM" ) | ( irtmodel[0] == "GPCM.design" ) ){
                    // xxf[items.temp,k] <- xxf[items.temp,k] * (k-1)^2
                    xxf( item_ii, kk) = xxf( item_ii, kk) * kk2 ;
                }
            }
        }
    }
    //--- OUTPUT
    return Rcpp::List::create(
            Rcpp::Named("xxf") = xxf,
            Rcpp::Named("xbar") = xbar,
            Rcpp::Named("xbar2") = xbar2,
            Rcpp::Named("xtemp") = xtemp
        ) ;
}
///********************************************************************
