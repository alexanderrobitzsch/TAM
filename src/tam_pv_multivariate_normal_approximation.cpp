//// File Name: tam_pv_multivariate_normal_approximation.cpp
//// File Version: 0.22


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace arma;


///********************************************************************
///** tam_mvrnorm_rcpp
// [[Rcpp::export]]           
Rcpp::NumericMatrix tam_mvrnorm_rcpp(int n, Rcpp::NumericVector mu, Rcpp::NumericMatrix sigma) {
   int ncols = sigma.ncol();
   arma::vec Mu = Rcpp::as<arma::vec>(mu);
   arma::mat Sigma = Rcpp::as<arma::mat>(sigma);
   arma::mat Y = arma::randn(n, ncols);
   arma::mat Z = arma::trans( arma::repmat(Mu, 1, n) ) + Y * arma::chol(Sigma);
   Rcpp::NumericMatrix z = Rcpp::wrap(Z);
   return z ;
}
///********************************************************************



//**********************************************************************
//** weighted mean
//** tam_pv_weighted_mean
// [[Rcpp::export]] 
Rcpp::NumericVector tam_pv_weighted_mean( Rcpp::NumericMatrix theta, Rcpp::NumericVector wgt)
{
   int D = theta.ncol();
   int N = theta.nrow();
   Rcpp::NumericVector Mu(D);
   double y=0;
   for (int dd=0; dd<D; dd++){     
     y=0;     
     for (int nn=0; nn<N; nn++){
       y += wgt[nn]*theta(nn,dd);
       if (dd==0){
       }      
     }   
     Mu[dd] = y ;
   }
   return(Mu);
}


//**********************************************************************
//** weighted covariance
//** tam_pv_weighted_cov
// [[Rcpp::export]] 
Rcpp::List tam_pv_weighted_cov( Rcpp::NumericMatrix theta, Rcpp::NumericVector wgt)
{   
   int D = theta.ncol();
   int N = theta.nrow();
   Rcpp::NumericVector Mu = tam_pv_weighted_mean( theta, wgt);
   Rcpp::NumericMatrix covmat(D,D);   
   double y=0;
   double y1=0;         
   for (int dd=0; dd<D; dd++){
     for (int ee=dd; ee<D; ee++){
       y = 0;       
       for (int nn=0; nn<N; nn++){
            y += wgt[nn] * theta(nn,dd) * theta(nn,ee);
       }
       y1 = y - Mu[dd] * Mu[ee] ;
       covmat(dd,ee) = y1;
       covmat(ee,dd) = y1;
    }
   }    
   // OUTPUT                                
   return Rcpp::List::create(    
        Rcpp::Named("Mu") = Mu ,  
        Rcpp::Named("covmat") = covmat 
        ) ;  
}



//**********************************************************************
//** multidimensional theta sampling
//** tam_pv_sample_theta_multidim
// [[Rcpp::export]] 
Rcpp::NumericMatrix tam_pv_sample_theta_multidim( 
      Rcpp::NumericMatrix post, Rcpp::NumericMatrix theta)
{   
   Rcpp::List res;
   Rcpp::NumericVector wgt;
   Rcpp::NumericMatrix Z;
   int N = post.nrow();
   int D = theta.ncol();   
   Rcpp::NumericMatrix theta_samp(N, D);   
   int NS=1;
   for (int nn=0; nn<N; nn++){   
       wgt = post( nn, _);
       res = tam_pv_weighted_cov( theta, wgt) ;
       Z = tam_mvrnorm_rcpp( NS , res["Mu"] , res["covmat"] );
       theta_samp(nn, _) = Z(0, _) ; 
   }
   return theta_samp ;
}



