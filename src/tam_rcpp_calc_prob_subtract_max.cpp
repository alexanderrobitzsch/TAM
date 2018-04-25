//// File Name: tam_rcpp_calc_prob_subtract_max.cpp
//// File Version: 1.05

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
     for (int ii=0;ii<NI;ii++){
       for (int tt=0;tt<TP;tt++){
	     Mval = rr0M( ii , tt ) ;
	     for (int hh=1;hh<NK;hh++){
		   val = rr0M( hh*NI + ii , tt ) ;
		   if ( ! R_IsNA( val ) ){
			if ( val > Mval ){
			     Mval = val ;
			}
		   }
	     } 
	     for (int hh=0;hh<NK;hh++){
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
     ///////////////////////////////////////  
     /// OUTPUT     
     return rr1M ;    
}
///********************************************************************

