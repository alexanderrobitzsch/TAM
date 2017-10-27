//// File Name: tam_modelfit_aux.cpp
//// File Version: 3.06


#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** tam_q3_calc_V2q3jack
// [[Rcpp::export]]           
Rcpp::List tam_q3_calc_V2q3jack( Rcpp::NumericMatrix residM, 
	Rcpp::NumericMatrix resp_ind ){

     int I=residM.ncol();  
     int N=residM.nrow();  
     int RR=I*(I-1)/2 ;  
     int JJ=0;  
     Rcpp::NumericMatrix dfr(RR,4) ;  
     Rcpp::NumericMatrix sumresidii1(RR,JJ+1) ;  
     Rcpp::NumericMatrix sumresidii2(RR,JJ+1) ;  
     Rcpp::NumericMatrix sumresidsqii1(RR,JJ+1) ;  
     Rcpp::NumericMatrix sumresidsqii2(RR,JJ+1) ;  
     Rcpp::NumericMatrix sumresidii1ii2(RR,JJ+1) ;  
     Rcpp::NumericMatrix nresid(RR,JJ+1) ;  
       
     // Rcpp::NumericMatrix q3jack(RR,JJ);  
     // Rcpp::NumericMatrix aq3jack(RR,JJ);  
       
     double mii1=0;  
     double mii2=0;  
     double sdii1=0;  
     double sdii2=0;  
     double covii1ii2=0;         
     double tmp1=0;  
     double t1=0;  
     int rr=0;  
     int nrr=0;  
       
     for (int ii1=0; ii1 < I-1;ii1++){   
     for (int ii2=ii1+1; ii2<I;ii2++){  
       
     for (int nn=0;nn<N;nn++){  
     if ( ( resp_ind(nn,ii1)==1 ) & ( resp_ind(nn,ii2)==1) ){  
          sumresidii1(rr,0) += residM(nn,ii1) ;	  
          sumresidii2(rr,0) += residM(nn,ii2) ;  
          t1 = residM(nn,ii1)*residM(nn,ii1) ;  
          sumresidsqii1(rr,0) += t1;  
          t1 = residM(nn,ii2)*residM(nn,ii2) ;  
          sumresidsqii2(rr,0) += t1;  
          t1 = residM(nn,ii1)*residM(nn,ii2) ;  
          sumresidii1ii2(rr,0) += t1 ;  
          nresid(rr,0) ++ ;  
     	}  
     	}	  
     // item indices  
     dfr(rr,0)=ii1+1;  
     dfr(rr,1)=ii2+1;  
     //*** calculate correlation  
     nrr = nresid(rr,0);  
     // means  
     mii1 = sumresidii1(rr,0) / nrr ;  
     mii2 = sumresidii2(rr,0) / nrr ;  
     // covariance  
     covii1ii2 = sumresidii1ii2(rr,0) - nrr * mii1*mii2 ;  
     covii1ii2 = covii1ii2 / ( nrr - 1 ) ;  
     // standard deviations  
     sdii1 = sumresidsqii1(rr,0) - nrr * mii1*mii1 ;  
     sdii1 = sqrt( sdii1 / ( nrr - 1 ) ) ;  
     sdii2 = sumresidsqii2(rr,0) - nrr * mii2*mii2 ;  
     sdii2 = sqrt( sdii2 / ( nrr - 1 ) ) ;  
     // compute correlation  
     dfr(rr,2) = covii1ii2 / sdii1 / sdii2 ;  
     tmp1 += dfr(rr,2) ;  
     rr ++ ;  
     // Rcpp::Rcout << "tmp1 " <<  tmp1 <<  std::flush << std::endl ;  
     }  
     }  
       
     //*****************************  
     // compute adjusted Q3 statistic  
     double mQ3 = tmp1 / RR ;  
     for (rr=0;rr<RR;rr++){  
          dfr(rr,3) = dfr(rr,2) - mQ3 ;  
             	}  
     //*************************************************      
     // OUTPUT                                 
     return Rcpp::List::create(   
           Rcpp::_["dfr"] = dfr   
         ) ;    
}


///********************************************************************
///** tam_q3_calc_V2counts
// [[Rcpp::export]]           
Rcpp::List tam_q3_calc_V2counts( Rcpp::NumericMatrix resp0, 
	Rcpp::NumericMatrix resp_ind, Rcpp::NumericVector rprobs, 
	Rcpp::NumericMatrix hwt, Rcpp::NumericVector maxKi, int maxK ){
        
     int I=resp0.ncol();  
     int N=resp0.nrow();  
     int RR = I * (I-1) / 2 ;  
     int TP=hwt.ncol();  
     int maxK2 = maxK*maxK ;  
     double g1=0;  
     Rcpp::NumericMatrix obs_counts(RR,maxK*maxK);  
     Rcpp::NumericMatrix exp_counts(RR,maxK*maxK);  
     Rcpp::NumericVector hwtii(TP);  
     Rcpp::NumericVector hwtiifull(TP);  
     Rcpp::NumericMatrix maxKiM(RR,5);  
     // calculate hwtii with full item responses  
     for (int tt=0;tt<TP;tt++){  
        for (int nn=0;nn<N;nn++){  
     	    hwtiifull[tt] += hwt(nn,tt) ;  
     		} // end nn  
     }  // end tt  
     int rr=0;  
     for (int ii1=0; ii1 < I -1 ; ii1++){  
     for (int ii2=ii1+1;ii2 <I;ii2++){  
       
     for (int tt=0;tt<TP;tt++){  
     	hwtii[tt] = hwtiifull[tt] ;  
     	//**** expected counts  
     	for (int nn=0;nn<N;nn++){  
     	if ( ( resp_ind(nn,ii1)==0 ) | ( resp_ind(nn,ii2)==0 ) ){  
     	    hwtii[tt] = hwtii[tt] - hwt(nn,tt) ;  
     			}  
     		} // end nn  
     }  // end tt  
       
     for (int kk1=0;kk1<maxKi[ii1]+1;kk1++){  
     for (int kk2=0;kk2<maxKi[ii2]+1;kk2++){  
     for (int tt=0;tt<TP;tt++){  
       g1 = hwtii[tt]*rprobs[ ii1 + kk1*I + tt*I*maxK ] * rprobs[ ii2 + kk2*I + tt*I*maxK ] ;  
       exp_counts(rr, kk1+kk2*maxK) += g1 ;  
           	}  
       
     //**** observed counts  
     // t1 <- sum( resp.ind[,ii1]*(resp0[,ii1]==kk1)*resp.ind[,ii2]*(resp0[,ii2]==kk2) )  
     // obs_counts[rr, (kk1+1) + kk2*maxK ] <- t1  
       
     for (int nn=0;nn<N;nn++){  
     if ( ( resp_ind(nn,ii1)==1 ) & ( resp_ind(nn,ii2)==1 ) ){  
      if ( ( resp0(nn,ii1)==kk1 ) & ( resp0(nn,ii2) == kk2 ) ){ 	  
      	 obs_counts(rr, kk1+kk2*maxK) ++ ;  
       		}  
     		}  
     	} // end nn  
     }  // end kk1  
     }  // end kk2  
       
     maxKiM(rr,0) = ii1+1 ;  
     maxKiM(rr,1) = ii2+1 ;  
     maxKiM(rr,2) = maxKi[ii1] ;  
     maxKiM(rr,3) = maxKi[ii2] ;  
     maxKiM(rr,4) = maxKi[ii1]*maxKi[ii2];  
     rr ++ ;  
     }  
     }  
     //*************************************************      
     // OUTPUT                                 
     return Rcpp::List::create(   
         Rcpp::_["obs_counts"] = obs_counts ,  
         Rcpp::_["exp_counts"] = exp_counts ,  
         Rcpp::_["maxKiM"] = maxKiM ,  
         Rcpp::_["RR"] = RR ,  
         Rcpp::_["maxK"] = maxK ,  
         Rcpp::_["maxK2"] = maxK2  
         ) ;    
}


///********************************************************************
///** tam_calccov
// [[Rcpp::export]]           
Rcpp::List tam_calccov( Rcpp::NumericMatrix counts, 
	Rcpp::NumericMatrix scorematrix, Rcpp::NumericVector adjust ){
             
     int RR=counts.nrow();  
     int NC=counts.ncol();  
     Rcpp::NumericVector cov_ij(RR);  
     Rcpp::NumericVector cor_ij(RR);  
     Rcpp::NumericVector n_ij(RR);  
     Rcpp::NumericMatrix mean_ij(RR,2);  
     Rcpp::NumericMatrix sd_ij(RR,2);  
     double crrcc=0;  
             
     for (int rr=0;rr<RR;rr++){  
       
     for (int cc=0;cc<NC;cc++){  
        crrcc = counts(rr,cc) ;  
        n_ij[rr] += crrcc ;  
        mean_ij(rr,0) += scorematrix(cc,0)*crrcc;  
        mean_ij(rr,1) += scorematrix(cc,1)*crrcc;  
        sd_ij(rr,0) += pow(scorematrix(cc,0),2.0)*crrcc;  
        sd_ij(rr,1) += pow(scorematrix(cc,1),2.0)*crrcc;  
        cov_ij[rr] += crrcc*scorematrix(cc,0)*scorematrix(cc,1);  
        		}  
     // calculate means  
     for (int kk=0;kk<2;kk++){  
       mean_ij(rr,kk) = mean_ij(rr,kk) / n_ij[rr] ;  
       sd_ij(rr,kk) = sd_ij(rr,kk) - n_ij[rr] * pow(mean_ij(rr,kk),2);  
       sd_ij(rr,kk) = sqrt( sd_ij(rr,kk) / ( n_ij[rr] - adjust[0] ) ) ;  
       	}  
      cov_ij[rr] = cov_ij[rr] - n_ij[rr] * mean_ij(rr,0)*mean_ij(rr,1) ;  
      cov_ij[rr] = cov_ij[rr] / ( n_ij[rr] - adjust[0] ) ;  
      cor_ij[rr] = cov_ij[rr] / sd_ij(rr,0) / sd_ij(rr,1) ;         
      }  

     //*************************************************      
     // OUTPUT                 
     return Rcpp::List::create(   
         Rcpp::_["cov_ij"] = cov_ij ,  
         Rcpp::_["cor_ij"] = cor_ij ,  
         Rcpp::_["N_ij"] = n_ij ,  
         Rcpp::_["M_ij"] = mean_ij ,  
         Rcpp::_["SD_ij"] = sd_ij  
         ) ;    
}

///********************************************************************
///** tam_q3_calc_residM
// [[Rcpp::export]]           
Rcpp::List tam_q3_calc_residM( Rcpp::NumericVector rprobs, 
	Rcpp::NumericMatrix resp, int I, int TP, int maxK, 
	Rcpp::NumericVector maxKi, Rcpp::NumericMatrix hwt ){

     int N = resp.nrow();  
     Rcpp::NumericMatrix residM(N,I) ;  
       
     // for (ii in 1:I){  
     //  #ii <- 1  
     //  for (kk in 1:(maxKi[ii]) ){  
     //	# kk <- 1  
     //	rp.kk <- rprobs[ii,kk+1,]    
     //	v1 <- hwt * matrix( rp.kk   , nrow=N , ncol=TP , byrow=TRUE )  
     //	residM[,ii] <- residM[,ii] + rowSums(v1)*kk    
     //			}  
     //		}  
     for (int nn=0;nn < N ; nn++){  
     for (int ii=0; ii < I ; ii++){  
     for (int kk=1;kk<maxKi[ii]+1;kk++){  
       for (int tt=0;tt<TP;tt++){  
          residM(nn,ii) += kk*hwt(nn,tt)*rprobs(ii+kk*I+tt*I*maxK) ;  
              	}  
     	}	  
         }  
        }  
        	  
     //*************************************************      
     // OUTPUT                                 
     return Rcpp::List::create(   
         Rcpp::_["residM"] = residM        
         ) ;    
}


