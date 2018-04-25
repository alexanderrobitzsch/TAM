//// File Name: tam_rcpp_rowcumsums.cpp
//// File Version: 3.08


#include <Rcpp.h>

using namespace Rcpp;


//# The C code was posted by Romain Francois at
//# http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2010-October/001198.html

///********************************************************************
///** tam_rcpp_rowCumsums
// [[Rcpp::export]]           
Rcpp::NumericMatrix tam_rcpp_rowCumsums( Rcpp::NumericMatrix input )
{     
     Rcpp::NumericMatrix output  = Rcpp::clone< Rcpp::NumericMatrix >(input);  
     int nr = input.nrow();
     int nc = input.ncol() ;  
     Rcpp::NumericVector tmp(nr);  
     for( int i=0; i<nc; i++){  
         tmp = tmp + input.column(i) ;  
         Rcpp::NumericMatrix::Column target( output, i ) ;  
         std::copy( tmp.begin(), tmp.end(), target.begin() ) ;  
     }  
     return output ;
}
///********************************************************************



