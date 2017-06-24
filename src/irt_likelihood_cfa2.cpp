


#include <Rcpp.h>

using namespace Rcpp;


// user includes



///********************************************************************
///** irt_likelihood_cfa2
// [[Rcpp::export]]   
Rcpp::List irt_likelihood_cfa2( Rcpp::NumericMatrix data, 
	Rcpp::NumericVector nu, Rcpp::NumericMatrix psi, 
	Rcpp::NumericMatrix L, Rcpp::NumericMatrix theta ){
 
     int N = data.nrow();  
     int I = data.ncol();  
     int D = L.ncol();  
     int TP= theta.nrow() ;  
       
     Rcpp::NumericMatrix hwt(N,TP);  
     std::fill( hwt.begin(), hwt.end() , 1 ) ;  
       
     double term=0;  
     double val=0;  
     double sdii=0;  
     // double t1=0;  
       
     for (int tt=0;tt<TP;tt++){  
     for (int ii=0;ii<I;ii++){  
         term = nu[ii] ;  
         for (int dd=0; dd < D ; dd++){  
     		   term += L(ii,dd) * theta(tt,dd) ;  
     					}  
        sdii = sqrt( psi(ii,ii) ) ;  
          for (int nn=0;nn<N;nn++){		  
     	if ( ! R_IsNA( data(nn,ii) ) ){		  
     //		val = R::dnorm( data(nn,ii) , term , sdii , false ) ;  
                     val = Rf_dnorm4( data(nn,ii) , term , sdii , false ) ;  
     //		t1 = data(nn,ii) - term ;  
     //                val = exp( - t1*t1 / 2 / psi(ii,ii) ) ;  
     //                val = val / sqrt( 2 * PI ) / sdii ;  
     		hwt(nn,tt) = hwt(nn,tt) * val ;  
     		}    // end if not missing  
     	}  // end nn  
        } // end ii  
      }   // end tt	  
     		  
     //*************************************************      
     // OUTPUT                                
      return Rcpp::List::create(    
         Rcpp::_["hwt"] = hwt ,    
         Rcpp::_["N"] = N , 
         Rcpp::_["I"] = I , 
         Rcpp::_["TP"]=TP , 
         Rcpp::_["D"] = D  
         ) ;  
}
///********************************************************************



