

#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** tam_fit_simul
// [[Rcpp::export]]           
Rcpp::List tam_fit_simul( Rcpp::NumericMatrix rn1M, 
	Rcpp::NumericMatrix c_hwt, Rcpp::NumericMatrix Ax, 
	Rcpp::NumericMatrix xbar, Rcpp::NumericMatrix var1, 
	Rcpp::NumericMatrix Uz2, Rcpp::NumericMatrix Vz2, 
	Rcpp::NumericVector nstud_ip, Rcpp::NumericVector pweights ){

     int N = rn1M.nrow() ;  
     int Nsimul = rn1M.ncol() ;  
     int TP=c_hwt.ncol();  
     Rcpp::NumericVector j(N);  
     Rcpp::NumericVector wt_numer(N);  
     Rcpp::NumericVector wt_denom(N);  
     Rcpp::NumericVector z2(N);  
     Rcpp::NumericVector wt_var(N);  
     Rcpp::NumericVector varz2(N);  
     Rcpp::NumericVector Outfit_SIM(Nsimul);  
     Rcpp::NumericVector Infit_SIM(Nsimul);  
     Rcpp::NumericVector Infit_t_SIM(Nsimul);  
     Rcpp::NumericVector Outfit_t_SIM(Nsimul);  
       
     double rn=0;  
     double tmp1=0;  
     double tmp2=0;  
     double tmp3a=0;  
     double tmp3b=0;  
     double tmp3c=0;  
     double tmp4=0;  
     double eps = 1E-10;  
     int jj=0;  
     double vf=0;  
     double ot= 1/3.0 ;  
       
     for (int hh=0 ;hh<Nsimul;hh++){  
       
     // draws for every person n  
     for (int nn=0;nn<N;nn++){  
     	jj=0;  
     	rn = rn1M(nn,hh);  
     	for (int tt=0;tt<TP;tt++){  
     	if ( rn < c_hwt(nn,tt) ){  
     		j[nn] = jj ;  
     		break ;  
     		} else { jj ++ ; }  
     	}  
     }  
       
     // Ax and xbar  
     for (int nn=0;nn<N;nn++){  
        tmp1 = Ax(nn,j[nn]) - xbar(nn,j[nn] );	  
        wt_numer[nn] = tmp1 * tmp1 ;  
        wt_denom[nn] = var1(nn,j[nn]) ;  
        z2[nn] = wt_numer[nn] / ( wt_denom[nn] + eps ) ;  
        //    varz2 <- Uz2[s]  
        //    wt_var <- Vz2[s]   
        varz2[nn] = Uz2(nn,j[nn]) ;  
        wt_var[nn] = Vz2(nn,j[nn]) ;  
        		}  
       
     // calculation of fit statistics  
       
     //      Outfit[p] <- sum( z2*pweights, na.rm = TRUE  ) / nstud.ip  
     //      Outfit_SIM[hh] <- Outfit[p] 
     //  Infit[p] <- sum( wt_numer*pweights,na.rm = TRUE )  
     //       /sum(wt_denom*pweights,na.rm = TRUE  )         
     tmp2=0;  
     tmp3a=0;  
     tmp3b=0;  
     tmp3c=0;  
     tmp4=0;  
     for (int nn=0; nn <N;nn++){  
     	if ( ! R_IsNA(z2[nn] ) ){  
     		tmp2 += z2[nn] * pweights[nn] ;  
     	        tmp3a += wt_numer[nn] * pweights[nn] ;  
     	        tmp3b += wt_denom[nn] * pweights[nn] ;  
                     tmp3c += wt_var[nn] * pweights[nn] ;  
                     tmp4 += varz2[nn] * pweights[nn] ;		  
     				}  
     			}  
     Outfit_SIM[hh] = tmp2 / nstud_ip[0] ;  
     Infit_SIM[hh] = tmp3a / tmp3b ;   		  
       
     // #Infit t  
     // vf <- sum(wt_var*pweights,na.rm = TRUE )/(sum(wt_denom*pweights,na.rm = TRUE)^2 )   
     // Infit_t[p] <- (Infit[p]^(1/3)-1) * 3/sqrt(vf) + sqrt(vf)/3    
       
     vf = tmp3c / ( tmp3b*tmp3b ) ;  
     Infit_t_SIM[hh] = ( pow( Infit_SIM[hh] , ot ) - 1 ) * 3 / sqrt(vf) + sqrt(vf)/3;  
       
     // #Outfit t  
     //  vf2 <- sum(varz2*pweights,na.rm = TRUE )/(nstud.ip^2)  
     //  Outfit_t[p] <- (Outfit[p]^(1/3)-1) * 3/sqrt(vf2) + sqrt(vf2)/3  
       
     vf = tmp4 / ( nstud_ip[0] * nstud_ip[0] ) ;  
     Outfit_t_SIM[hh] = ( pow( Outfit_SIM[hh] , ot ) - 1 ) * 3 / sqrt(vf) + sqrt(vf)/3;  
       
     }  
       
     //*************************************************      
     // OUTPUT              
                   
      return Rcpp::List::create(    
         Rcpp::_["Outfit_SIM"] = Outfit_SIM ,  
         Rcpp::_["Infit_SIM"] = Infit_SIM ,  
         Rcpp::_["Infit_t_SIM"] = Infit_t_SIM ,  
         Rcpp::_["Outfit_t_SIM"] = Outfit_t_SIM  
         ) ;  
}



