//// File Name: tam_rcpp_prior_normal_density.cpp
//// File Version: 3.14

#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///-- normal density with different person means
///** tam_rcpp_prior_normal_density_unequal_means
// [[Rcpp::export]]
Rcpp::NumericMatrix tam_rcpp_prior_normal_density_unequal_means(
    Rcpp::NumericMatrix theta, Rcpp::NumericMatrix mu, Rcpp::NumericMatrix varInverse,
    Rcpp::NumericVector COEFF )
{

    int nnodes = theta.nrow();
    int ndim = theta.ncol();
    int nstud = mu.nrow();
    double coeff = COEFF[0];
    Rcpp::NumericMatrix gwt(nstud,nnodes);
    Rcpp::NumericVector x1(ndim);

    for (int nn=0; nn<nstud; nn++){
        for (int qq=0;qq<nnodes;qq++){
            for (int dd=0;dd<ndim;dd++){
                x1[dd] = theta(qq,dd) - mu(nn,dd);
            }
            for (int dd1=0;dd1<ndim;dd1++){  // beg dd1
                gwt(nn,qq) += x1[dd1]*x1[dd1] * varInverse(dd1,dd1);
                for (int dd2=(dd1+1); dd2<ndim; dd2++){ // beg dd2
                    gwt(nn,qq) += 2*x1[dd1]*x1[dd2] * varInverse(dd1,dd2);
                }   // end dd2
            }  // end dd1
            gwt(nn,qq) = coeff * exp( -0.5*gwt(nn,qq) );
        } // end qq
    } // end nn

    //// OUTPUT
    return gwt;
}
//*******************************************************************


///********************************************************************
///-- normal density with equal mean for all subjects
///** tam_rcpp_prior_normal_density_equal_means
// [[Rcpp::export]]
Rcpp::NumericVector tam_rcpp_prior_normal_density_equal_means(
    Rcpp::NumericMatrix theta, Rcpp::NumericMatrix mu, Rcpp::NumericMatrix varInverse,
    Rcpp::NumericVector COEFF )
{

    int nnodes = theta.nrow();
    int ndim = theta.ncol();
    double coeff = COEFF[0];
    Rcpp::NumericVector gwt(nnodes);
    Rcpp::NumericVector x1(ndim);

    int nn=0;
    for (int qq=0;qq<nnodes;qq++){
        for (int dd=0;dd<ndim;dd++){
            x1[dd] = theta(qq,dd) - mu(nn,dd);
        }
        for (int dd1=0;dd1<ndim;dd1++){  // beg dd1
            gwt[qq] += x1[dd1]*x1[dd1] * varInverse(dd1,dd1);
            for (int dd2=(dd1+1);dd2<ndim;dd2++){ // beg dd2
                gwt[qq] += 2*x1[dd1]*x1[dd2] * varInverse(dd1,dd2);
            }   // end dd2
        }  // end dd1
        gwt[qq] = coeff * exp( -0.5*gwt[qq] );
    } // end qq

    //// OUTPUT
    return gwt;
}
///********************************************************************

