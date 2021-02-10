//// File Name: tam_rcpp_tam_jml.cpp
//// File Version: 0.214


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

using namespace Rcpp;
using namespace arma;




///********************************************************************
///** tam_rcpp_tam_jml_calc_probs
// [[Rcpp::export]]
Rcpp::List tam_rcpp_tam_jml_calc_probs( Rcpp::NumericVector theta,
        Rcpp::NumericMatrix AXsi, Rcpp::NumericMatrix B, int maxK,
        Rcpp::LogicalMatrix resp_ind)
{
    int N = theta.size();
    int I = AXsi.nrow();

    // R> print( str(rprobsWLE) )
    // num [1:127, 1:2, 1:4752]
    int H=I*maxK*N;
    Rcpp::NumericVector probs(H);
    Rcpp::NumericVector ptemp(maxK);
    double st=0;

    for (int nn=0;nn<N; nn++){
        for (int ii=0; ii<I; ii++){
            if (resp_ind(nn,ii)){
                st=0;
                for (int kk=0; kk<maxK; kk++){
                    ptemp[kk]=std::exp(B(ii,kk)*theta[nn]+AXsi(ii,kk));
                    st+=ptemp[kk];
                }
                for (int kk=0; kk<maxK; kk++){
                    probs[ii+kk*I+nn*I*maxK] = ptemp[kk]/st;
                }
            }  // end resp_ind(nn,ii)=1
        }  // end ii
    }   // end nn

    //**** OUTPUT:
    return Rcpp::List::create(
            Rcpp::Named("theta") = theta,
            Rcpp::Named("probs") = probs
        );
}
///********************************************************************


///********************************************************************
///** tam_rcpp_tam_jml_wle_bbari
// [[Rcpp::export]]
Rcpp::List tam_rcpp_tam_jml_wle_bbari( Rcpp::NumericVector rprobs,
            Rcpp::NumericMatrix B1, Rcpp::NumericMatrix BB,
            int maxK, Rcpp::LogicalMatrix resp_ind)
{
    int N=resp_ind.nrow();
    int I=resp_ind.ncol();

    Rcpp::NumericMatrix B_bari(N,I);
    Rcpp::NumericMatrix BB_bari(N,I);

    for (int nn=0;nn<N; nn++){
        for (int ii=0; ii<I; ii++){
            if (resp_ind(nn,ii)){
                for (int kk=0; kk<maxK; kk++){
                    B_bari(nn,ii)+=B1(ii,kk)*rprobs[ii+kk*I+nn*I*maxK];
                    BB_bari(nn,ii)+=BB(ii,kk)*rprobs[ii+kk*I+nn*I*maxK];
                }
            }
        }
    }

    //**** OUTPUT:
    return Rcpp::List::create(
            Rcpp::Named("B_bari") = B_bari,
            Rcpp::Named("BB_bari") = BB_bari
        );
}
///********************************************************************



///********************************************************************
///** tam_rcpp_tam_jml_calc_xsi_rr
// [[Rcpp::export]]
Rcpp::List tam_rcpp_tam_jml_calc_xsi_rr( Rcpp::LogicalMatrix resp_ind,
        Rcpp::NumericVector rprobs, int maxK, Rcpp::NumericMatrix pweightsM)
{
    int N=resp_ind.nrow();
    int I=resp_ind.ncol();

    Rcpp::NumericMatrix r(I,maxK);
    Rcpp::NumericVector rr(I*maxK*maxK);

    // R> print( str(rprobs) )
    // num [1:127, 1:2, 1:4752] 0.00199 0.01602 0.04885 0.00232 0.14105 ...

    //    M1 <- resp_ind_sel*rp3.pweightsM
    //    t_rprobs <- aperm( rprobs, dim=c(3,2,1) )
    //    for (k1 in 1:maxK) {
    //        M1_k1 <- t_rprobs[,k1,] * M1
    //        r[,k1] <- colSums( M1_k1, na.rm=TRUE)
    //        for (k2 in 1:maxK) {
    //            rr[,k1,k2] <- colSums( M1_k1 * t_rprobs[,k2,], na.rm=TRUE)
    //        }
    //    }

    double t1=0;
    for (int ii=0; ii<I; ii++){
        for (int kk=0; kk<maxK; kk++){
            for (int nn=0; nn<N; nn++){
                if (resp_ind(nn,ii)){
                    t1=rprobs[ii+kk*I+nn*I*maxK]*pweightsM(nn,ii);
                    r(ii,kk) += t1;
                    for (int hh=0; hh<maxK; hh++){
                        rr[ii+kk*I+hh*I*maxK] += t1*rprobs[ii+hh*I+nn*I*maxK];
                    }  // end hh
                }  // end resp_ind(nn,ii)
            }  // end nn
        } // end kk
    }  // end ii

    //**** OUTPUT:
    return Rcpp::List::create(
            Rcpp::Named("r") = r,
            Rcpp::Named("rr") = rr
        );
}
///********************************************************************
