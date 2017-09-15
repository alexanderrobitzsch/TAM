//// File Name: tamwle_helper_ccode.cpp
//// File Version: 3.03
//// File Last Change: 2017-02-18 21:37:15

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** kernelplsaux
// [[Rcpp::export]]           
Rcpp::List tam_wle_Bs( Rcpp::NumericMatrix RPROBS, 
	Rcpp::NumericMatrix RESPIND, Rcpp::NumericMatrix CBL, 
	Rcpp::NumericMatrix CBB, Rcpp::NumericMatrix CBBB, 
	int cndim, int cnitems, int cmaxK, int cnstud){
   
	    ///////////////////////////////////////////////////////////
	    // INPUT indices
	    int citstud = cnitems*cnstud;
	    
	    ////////////////////////////////////////////////////////////
	    // define output vectors
	    Rcpp::NumericMatrix B_bari (citstud, cndim);
	    Rcpp::NumericMatrix BB_bari (citstud, cndim*cndim);
	    Rcpp::NumericMatrix BBB_bari (citstud, cndim);
	    Rcpp::NumericMatrix B_Sq (citstud, cndim*cndim);
	    Rcpp::NumericMatrix B2_B (citstud, cndim);
	    Rcpp::NumericMatrix B_Cube (citstud, cndim);
	    
	    /////////////////////////////////////////////////////////
	    // CALCULATIONS
	    
	    for(int ii=0; ii<cnitems; ii++){// item loop
	    	for(int jj=0; jj<cnstud; jj++){// student loop
	    		for(int dd1=0; dd1<cndim; dd1++){// dimension loop 1
	    		
	    			B_bari( cnstud*ii+jj , dd1 ) = 0;
	    			BBB_bari( cnstud*ii+jj , dd1 ) = 0;
	    			for(int cc=0; cc<cmaxK; cc++){// category loop
	    				B_bari( cnstud*ii+jj , dd1 ) += CBL( cnitems*cc+ii , dd1 )*RPROBS( cnitems*cc+ii , jj )*RESPIND( jj , ii );
	    				BBB_bari( cnstud*ii+jj , dd1 ) += CBBB( cnitems*cc+ii , dd1 )*RPROBS( cnitems*cc+ii , jj )*RESPIND( jj , ii );
	    			}
	    			
	    			B2_B( cnstud*ii+jj , dd1 ) = 0;
	    			B_Cube( cnstud*ii+jj , dd1 ) = 0;
	    			for(int dd2=0; dd2<cndim; dd2++){// category loop
	    				BB_bari(cnstud*ii+jj , cndim*dd1+dd2 )=0;
	    				for(int cc=0; cc<cmaxK; cc++){// category loop
	    					BB_bari( cnstud*ii+jj , cndim*dd2+dd1 ) += CBB( cnitems*cc+ii , cndim*dd2+dd1 )*RPROBS( cnitems*cc+ii , jj )*RESPIND( jj , ii );
	    				}
	    				
	    				B_Sq( cnstud*ii+jj , cndim*dd2+dd1 ) = B_bari( cnstud*ii+jj , dd1 )*B_bari( cnstud*ii+jj , dd2 );
	    				B2_B( cnstud*ii+jj , dd1 ) += BB_bari( cnstud*ii+jj , cndim*dd2+dd1 )*B_bari( cnstud*ii+jj , dd2 );
	    				B_Cube( cnstud*ii+jj , dd1 ) += B_Sq( cnstud*ii+jj , cndim*dd2+dd1 )*B_bari( cnstud*ii+jj , dd2 );
	    				
	    			}
	    		}
	    	}
	    }
	    
	    ///////////////////////////////////////////////////////
	    ///////////// O U T P U T   ///////////////////////////	    
	    return Rcpp::List::create(
                    Rcpp::_["B_bari"]=B_bari, 
                    Rcpp::_["BB_bari"]=BB_bari, 
                    Rcpp::_["BBB_bari"]=BBB_bari,
                    Rcpp::_["B_Sq"]=B_Sq, 
                    Rcpp::_["B2_B"]=B2_B, 
                    Rcpp::_["B_Cube"]=B_Cube );
}

///********************************************************************
///** tam_wle_errinv
// [[Rcpp::export]]           
Rcpp::NumericMatrix tam_wle_errinv( Rcpp::NumericMatrix myERR, 
	int cndim, int cnstud ){

	////////////////////////////////////////////////////////////
	// define output vectors
	arma::mat ERR_j = arma::zeros(cndim, cndim);
	arma::mat ERR_j_inv;
	Rcpp::NumericMatrix ERR_inv (cnstud, cndim*cndim);
	
	/////////////////////////////////////////////////////////
	// CALCULATIONS
	for(int jj=0; jj<cnstud; jj++){// item loop
		for(int dd1=0; dd1<cndim; dd1++){// dimension loop 1
			for(int dd2=0; dd2<cndim; dd2++){// dimension loop 2
				ERR_j(dd1,dd2) = myERR(jj, dd1+dd2*cndim);
			}
		}
		
		ERR_j_inv = arma::mat( arma::inv(ERR_j) );
		
		for(int dd1=0; dd1<cndim; dd1++){// dimension loop 1
			for(int dd2=0; dd2<cndim; dd2++){// dimension loop 2
				ERR_inv(jj, dd1+dd2*cndim) = ERR_j_inv(dd1, dd2);
			}
		}
	}
	
	///////////////////////////////////////////////////////
	///////////// O U T P U T   ///////////////////////////
	return ERR_inv;
}
