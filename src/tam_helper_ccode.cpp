//// File Name: tam_helper_ccode.cpp
//// File Version: 3.11
//// File Last Change: 2017-05-31 17:23:28

// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// using namespace arma;




///********************************************************************
///** theta_sq_cpp
// [[Rcpp::export]]           
Rcpp::NumericMatrix theta_sq_cpp( Rcpp::NumericMatrix theta ){
       
     int N = theta.nrow() ;  
     int D = theta.ncol() ;
     int D2 = D*D;          
     Rcpp::NumericMatrix thetasq(N,D2) ;  
       
     ////////////////////////////////////////  
     // calculation of squared theta matrix  
       
     // int nn = 0 ;  
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
///** interval_index_C
// [[Rcpp::export]]           
Rcpp::NumericVector interval_index_C( Rcpp::NumericMatrix MATR, 
	Rcpp::NumericVector RN ){

     int NR=MATR.nrow();  
     int NC=MATR.ncol();    
     // create output vectors  
     Rcpp::NumericVector IND (NR) ;  
     IND.fill(0);  
     for (int nn=0;nn<NR;++nn){  
      	for (int cc=0 ; cc < NC ; ++cc ){  
     	    if ( MATR(nn,cc) > RN[nn] ){  
     	    	    IND(nn) = cc + 1 ;  
     	    	    break ;   
     	    	           }  
     		}  
     	}  
     ///////////////////////////////////////  
     /// OUTPUT                  
     return IND;  
}



///********************************************************************
///** calc_prob_subtract_max
// [[Rcpp::export]]           
Rcpp::NumericMatrix calc_prob_subtract_max(	Rcpp::NumericMatrix rr0M , int NI , int NK , 
        int TP){
	
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

