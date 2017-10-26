//// File Name: tam_mml_mfr_helper.cpp
//// File Version: 3.01
//// File Last Change: 2017-02-18 20:16:25


#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** gresp_extend
// [[Rcpp::export]]           
Rcpp::NumericMatrix gresp_extend( Rcpp::NumericMatrix gresp, 
	Rcpp::NumericVector xstep ){

     int I=gresp.ncol() ;  
     int N=gresp.nrow();    
     Rcpp::NumericMatrix gresp2(N,I);  
       
     for (int ii=0;ii<I;ii++){  
     for (int nn=0;nn<N;nn++){  
     	if (! R_IsNA(gresp(nn,ii)) ) {  
     		if ( gresp(nn,ii) == xstep[ii] ){  
     			gresp2(nn,ii) = 1 ;   
     				}  
     				} else {  
     		gresp2(nn,ii) = NA_REAL ;  
     				}  
     		}  
     	}  
     return gresp2 ;
}


///********************************************************************
///** gresp_na_facets
// [[Rcpp::export]]           
Rcpp::NumericMatrix gresp_na_facets( Rcpp::NumericMatrix gresp, 
	Rcpp::CharacterVector rnfacets, Rcpp::CharacterVector rnx ){

     int I=gresp.ncol() ;  
     int N=gresp.nrow();    
     Rcpp::NumericMatrix gresp2 = gresp;  
       
     for (int ii=0;ii<I;ii++){  
     for (int nn=0;nn<N;nn++){  
     	if ( rnfacets[nn] != rnx[ii]   ) {  
     		gresp2(nn,ii) = NA_REAL ;  
     				}  
     		}  
     	}  
     // output
     return gresp2;  
}


///********************************************************************
///** a_matrix_cumsum
// [[Rcpp::export]]           
Rcpp::List a_matrix_cumsum( Rcpp::NumericMatrix index_matr, 
	Rcpp::NumericMatrix mm, int SG ){

     int NR = mm.nrow();  
     int NR1 = NR + SG;  
     int NC = mm.ncol() ;         
     Rcpp::NumericMatrix cumsum_mm(NR1,NC);  
     double tmp=0;  
     int ss =0;  
     int rr=0;  
       
     for (int cc=0; cc<NC ; cc++){  
     	ss = 2*SG;  
     	rr=0;	  
     	for( int zz=0; zz < NR ; zz++){   
     		if ( index_matr(zz,0) != ss ){  
     			rr ++ ;  
     			tmp = 0 ;  
     			      }  
     		tmp = tmp + mm( index_matr(zz,1) , cc ) ;  
     		cumsum_mm( rr , cc ) = tmp ;  
     		ss = index_matr(zz,0) ;  
     		rr ++ ;  
     				}	  
     		}  
       
     //*************************************************      
     // OUTPUT                                 
      return Rcpp::List::create(    
         Rcpp::_["index_matr"] = index_matr ,  
         Rcpp::_["SG"] = SG ,  
         Rcpp::_["cumsum_mm"] = cumsum_mm  
         ) ;  
}


///********************************************************************
///** colsums_gresp
// [[Rcpp::export]]           
Rcpp::NumericVector colsums_gresp( Rcpp::NumericMatrix gresp ){
              
     int NR = gresp.nrow();  
     int NC = gresp.ncol();         
     Rcpp::NumericVector sumnull(NC);  
     for (int cc=0;cc<NC;cc++){  
        sumnull[cc]=1;  
     	for (int nn=0;nn<NR;nn++){  
		if ( ! R_IsNA( gresp(nn,cc) ) ){  
		     if ( gresp(nn,cc) > 0 ){  
			     sumnull[cc] = 0 ;  
			     break;  
	        	}  
		}  
	}  
     }         
     return sumnull ;		  
}





