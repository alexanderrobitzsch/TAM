//// File Name: tam_pv_mcmc_rcpp.cpp
//// File Version: 0.26



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** tam_pv_mcmc_likelihood_Rcpp
// [[Rcpp::export]]           
Rcpp::NumericVector tam_pv_mcmc_likelihood_Rcpp( 
	Rcpp::NumericMatrix probs, Rcpp::NumericMatrix resp,
	Rcpp::LogicalMatrix resp_ind_bool, int maxK, int nstud, int nitems )
{
	Rcpp::NumericVector like(nstud); 
	like.fill(1);
	for (int nn=0; nn<nstud; nn++){
		for (int ii=0; ii<nitems; ii++){
			if ( resp_ind_bool(nn,ii) ){
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


