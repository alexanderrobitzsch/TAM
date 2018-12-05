//// File Name: tam_rcpp_pv_mcmc.cpp
//// File Version: 0.35



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** tam_rcpp_pv_mcmc_likelihood
// [[Rcpp::export]]
Rcpp::NumericVector tam_rcpp_pv_mcmc_likelihood(
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

///********************************************************************
///** tam_rcpp_pv_mcmc_calc_probs_irf_3pl
// [[Rcpp::export]]
Rcpp::List tam_rcpp_pv_mcmc_calc_probs_irf_3pl(
    Rcpp::NumericMatrix theta, Rcpp::NumericVector B, int I, int maxK,
    Rcpp::IntegerMatrix resp_ind, Rcpp::NumericMatrix AXsi  )
{
    int N = theta.nrow();
    int D = theta.ncol();
    int NBT = I*maxK*N;

    // Btheta[ items, categories, persons ]
    Rcpp::NumericVector Btheta(NBT);
    Rcpp::NumericVector rprobs(NBT);
    // B( items, categories, dimensions )
    int ind_temp = 0;
    double B_temp = 0;

    for (int nn=0; nn<N; nn++){  // person nn
        for (int ii=0; ii<I; ii++){   // item ii
            if( resp_ind(nn,ii) == 1 ){   // resp(nn,ii) == 1
                for (int kk=0; kk<maxK; kk++){  // category kk
                    ind_temp =  ii + kk*I + nn*I*maxK;
                    Btheta[ ind_temp ] = AXsi( ii, kk );
                    for (int dd=0; dd<D; dd++){   // dimension dd
                        B_temp = B[  ii + kk*I + dd*I*maxK ];
                        if ( B_temp != 0){
                            Btheta[ ind_temp ] = Btheta[ ind_temp ]  + B_temp * theta(nn,dd);
                        }  // end if B_temp
                    }    // end dd
                    Btheta[ ind_temp ] = std::exp( Btheta[ ind_temp ] );
                }   // end kk
            }   // end resp(nn,ii) == 1
        }  // end ii
    } // end nn

    // normalization
    double sum_temp=0;
    for (int nn=0; nn<N; nn++){  // person nn
        for (int ii=0; ii<I; ii++){   // item ii
            if( resp_ind(nn,ii) == 1 ){   // resp(nn,ii) == 1
                sum_temp=0;
                for (int kk=0; kk<maxK; kk++){  // category kk
                    ind_temp =  ii + kk*I + nn*I*maxK;
                    sum_temp = sum_temp + Btheta[ ind_temp ];
                }   // end kk
                for (int kk=0; kk<maxK; kk++){  // category kk
                    ind_temp =  ii + kk*I + nn*I*maxK;
                    rprobs[ind_temp] = Btheta[ ind_temp ] / sum_temp;
                }   // end kk
            }   // end resp(nn,ii) == 1
        }  // end ii
    } // end nn

    //*************************************************
    // OUTPUT
    return Rcpp::List::create(
                Rcpp::Named("rr") = Btheta,
                Rcpp::Named("rprobs") = rprobs
            );
}
///********************************************************************



// if ( ! R_IsNA( resp(nn,ii) ) ){
//                Rcpp::Rcout << "ii=" << ii << " " <<
//                    ii+resp(nn,ii)*nitems << " " << std::flush << std::endl;


