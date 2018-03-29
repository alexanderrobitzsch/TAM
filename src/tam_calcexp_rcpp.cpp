//// File Name: tam_calcexp_rcpp.cpp
//// File Version: 3.05


#include <Rcpp.h>

using namespace Rcpp;




///********************************************************************
///** TAM_CALCEXP
// [[Rcpp::export]]           
Rcpp::List TAM_CALCEXP( int NP, Rcpp::NumericMatrix rprobs, 
	Rcpp::NumericMatrix A, Rcpp::NumericMatrix INDEXIPNO, 
	Rcpp::NumericVector INDEXIPLIST2, Rcpp::NumericVector ESTXSIINDEX, 
	int C, Rcpp::NumericMatrix ITEMWT ){
         
     ////////////////////////////////////////////////////////////  
     // define output vectors  
     Rcpp::NumericVector XBAR (NP) ;  
     Rcpp::NumericVector XBAR2 (NP) ;  
     Rcpp::NumericVector XXF (NP) ;  
       
     ///////////////////////////////////////////////////////////  
     // DEFINE indices  
       
     int TP = rprobs.ncol();  
     int NEXI = ESTXSIINDEX.size() ;  
     int NR = rprobs.nrow();  
     int II = NR / C ;   
       
     /////////////////////////////////////////////////////////  
     // CALCULATIONS  
       
     // loop over xsi item parameters  
     for (int hh=0;hh<NEXI;++hh){  
       
     double pp = ESTXSIINDEX[hh] - 1 ;	  
     double ll = 0 ; // xbar element  
     double yy = 0 ; // xbar2 element  
     double zz = 0 ; // xxf element  
       
     // loop over theta points  
     for (int tt=0;tt<TP;++tt){  
     // loop over items within an item parameter group  
     double GG = INDEXIPNO(pp,1) - INDEXIPNO(pp,0) + 1 ;  
     for (int gg=0;gg<GG;++gg){  
        double ii=INDEXIPLIST2[ INDEXIPNO(pp,0)-1 + gg ]-1 ;	  
        // loop over categories cc = 1, ... , C  
        double vv1 = 0 ;  // xbar counter  
        double vv2 = 0 ;  // xxf counter  
        for (int cc=0;cc<C;++cc){     
     	// xbar calculation  
     	vv1 += A( ii+cc*II , pp) * rprobs( ii + cc*II , tt ) ;  
     	vv2 += A( ii+cc*II , pp) * A( ii+cc*II , pp) * rprobs( ii + cc*II , tt ) ;	  
     		}  
        ll += vv1*ITEMWT(tt,ii) ; // xbar addition  
        yy += vv1*vv1*ITEMWT(tt,ii) ; // xbar2 addition  
        zz += vv2*ITEMWT(tt,ii) ; // xxf addition     
        		}  
     	}	  
     XBAR[pp] = ll ;  
     XBAR2[pp] = yy ;  
     XXF[pp] = zz ;  
     	} // end xsi index  
       
     ///////////////////////////////////////////////////////  
     ///////////// O U T P U T   ///////////////////////////         
     return Rcpp::List::create(
           Rcpp::_["xbar"]=XBAR , 
           Rcpp::_["xbar2"]=XBAR2 , 
           Rcpp::_["xxf"]=XXF  
                ); 
}
///********************************************************************



///********************************************************************
///** TAM_CALCEXP2
// [[Rcpp::export]]           
Rcpp::List TAM_CALCEXP2( int NP, Rcpp::NumericVector rprobs, 
	Rcpp::NumericVector A, Rcpp::NumericMatrix INDEXIPNO, 
	Rcpp::NumericVector INDEXIPLIST2, Rcpp::NumericVector ESTXSIINDEX, 
	int C, Rcpp::NumericMatrix ITEMWT , int NR , int TP){
  
     ////////////////////////////////////////////////////////////  
     // define output vectors  
     NumericVector XBAR (NP) ;  
     NumericVector XBAR2 (NP) ;  
     NumericVector XXF (NP) ;  
       
     ///////////////////////////////////////////////////////////  
     // DEFINE indices  
     int NEXI = ESTXSIINDEX.size() ;  
     int II = NR / C ;   
       
     /////////////////////////////////////////////////////////  
     // CALCULATIONS  
       
     // loop over xsi item parameters  
     for (int hh=0;hh<NEXI;++hh){  
       
     double pp = ESTXSIINDEX[hh] - 1 ;	  
     double ll = 0 ; // xbar element  
     double yy = 0 ; // xbar2 element  
     double zz = 0 ; // xxf element  
       
     // loop over theta points  
     for (int tt=0;tt<TP;++tt){  
     // loop over items within an item parameter group  
     double GG = INDEXIPNO(pp,1) - INDEXIPNO(pp,0) + 1 ;  
     for (int gg=0;gg<GG;++gg){  
        double ii=INDEXIPLIST2[ INDEXIPNO(pp,0)-1 + gg ]-1 ;	  
        // loop over categories cc = 1, ... , C  
        double vv1 = 0 ;  // xbar counter  
        double vv2 = 0 ;  // xxf counter  
        for (int cc=0;cc<C;++cc){     
     	// xbar calculation  
     	vv1 += A[ ii+cc*II + pp*II*C ] * rprobs[ ii + cc*II + tt*II*C ] ;  
     	vv2 += A[ ii+cc*II + pp*II*C] * A[ ii+cc*II + pp*II*C ] * 
     					rprobs[ ii + cc*II  + tt*II*C ] ;	  
     		}  
        ll += vv1*ITEMWT(tt,ii) ; // xbar addition  
        yy += vv1*vv1*ITEMWT(tt,ii) ; // xbar2 addition  
        zz += vv2*ITEMWT(tt,ii) ; // xxf addition     
        		}  
     	}	  
     XBAR[pp] = ll ;  
     XBAR2[pp] = yy ;  
     XXF[pp] = zz ;  
     	} // end xsi index  
       
     ///////////////////////////////////////////////////////  
     ///////////// O U T P U T   ///////////////////////////         
     return Rcpp::List::create(
                Rcpp::_["xbar"]=XBAR , 
                Rcpp::_["xbar2"]=XBAR2 , 
                Rcpp::_["xxf"]=XXF  
                    ); 
}



///********************************************************************
///** redefine_vector_na
// [[Rcpp::export]]           
Rcpp::NumericVector redefine_vector_na( Rcpp::NumericVector A, 
	double val ){

     int N = A.size();    
     Rcpp::NumericVector A1(N);         
     for( int nn=0;nn<N;nn++){  
	     if ( R_IsNA( A[nn] ) ){  
		A1[nn] = val ;  
	     } else {  
		A1[nn] = A[nn];  
             }  
     }    
     //*************************************************      
     // OUTPUT              
     return A1;
}



