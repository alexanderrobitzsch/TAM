//// File Name: tam_pv_mcmc.cpp
//// File Version: 0.22
//// File Last Change: 2017-05-31 17:11:04



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** tam_pv_mcmc_likelihood_Rcpp
// [[Rcpp::export]]           
Rcpp::NumericVector tam_pv_mcmc_likelihood_Rcpp( 
	Rcpp::NumericMatrix probs, Rcpp::NumericMatrix resp,
	Rcpp::IntegerMatrix resp_ind , 
	int maxK, int nstud, int nitems ){
		
     Rcpp::NumericVector like(nstud); 
     like.fill(1);
     for (int nn=0; nn<nstud; nn++){
        // int nn=1;
        for (int ii=0; ii<nitems; ii++){
           if ( resp_ind(nn,ii) == 1 ){
           	 like[nn] = like[nn]*probs(nn, ii + resp(nn,ii)*nitems);
           }
        }
     }      
     //-------- OUTPUT              
     return like;  
}
///********************************************************************


// if ( ! R_IsNA( resp(nn,ii) ) ){
//           	 Rcpp::Rcout << "ii=" << ii << " " << 
//           	 	ii+resp(nn,ii)*nitems << " " << std::flush << std::endl ;            	 


