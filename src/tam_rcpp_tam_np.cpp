//// File Name: tam_rcpp_tam_np.cpp
//// File Version: 0.252



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** tam_rcpp_tam_np_posterior
// [[Rcpp::export]]
Rcpp::List tam_rcpp_tam_np_posterior( Rcpp::IntegerMatrix dat2,
    Rcpp::LogicalMatrix dat_resp, Rcpp::NumericVector probs0,
    Rcpp::NumericVector pi_k, Rcpp::NumericVector pweights, int K1 )
{
    int TP = pi_k.size();
    int N = dat2.nrow();
    int I = dat2.ncol();
    Rcpp::NumericMatrix fyiqk(N,TP);
    Rcpp::NumericMatrix fqkyi(N,TP);
    fyiqk.fill(1);
    Rcpp::NumericVector nik(I*K1*TP);
    Rcpp::NumericVector ll_individual(N);
    Rcpp::NumericVector probs(I*K1*TP);
    Rcpp::NumericVector Nik(I*TP);
    nik.fill(0);
    Nik.fill(0);
    double val = 0;
    double ll = 0;
    double temp = 0;
    double eps0 = 1e-100;
    double temp1 = 0;

    for (int nn=0; nn<N; nn++){
        // likelihood
        for (int ii=0; ii<I; ii++){
            if ( dat_resp(nn,ii) ){
                for (int tt=0; tt<TP; tt++){
                    fyiqk(nn,tt) *= probs0[ ii + dat2(nn,ii)*I + tt*I*K1 ];
                }
            }
        }
        // posterior
        val = 0;
        for (int tt=0; tt<TP; tt++){
            fqkyi(nn,tt) = fyiqk(nn,tt)*pi_k[tt];
            val += fqkyi(nn,tt);
        }
        temp1 = std::log( val + eps0 );
        ll_individual[nn] = temp1;
        ll += pweights[nn] * temp1;
        for (int tt=0; tt<TP; tt++){
            fqkyi(nn,tt) /= val;
        }
        // expected counts
        for (int ii=0; ii<I; ii++){
            if ( dat_resp(nn,ii) ){
                for (int tt=0; tt<TP; tt++){
                    temp = pweights[nn]*fqkyi(nn,tt);
                    nik[ ii + dat2(nn,ii)*I + tt*I*K1 ] += temp;
                    Nik[ ii + tt*I ] += temp;
                }
            }
        }
    }

    // probabilities
    double eps=1e-50;
    double Nik_eps = 0;
    int index = 0;
    for (int ii=0; ii<I; ii++){
        for (int tt=0; tt<TP; tt++){
            Nik_eps = Nik[ ii + tt*I ] + 2*eps;
            for (int kk=0; kk<K1; kk++){
                index = ii + kk*I + tt*I*K1;
                probs[ index ] = ( nik[ index ] + eps ) / Nik_eps;
            }
        }
    }

    //-------- OUTPUT
    return Rcpp::List::create(
                Rcpp::Named("fyiqk") = fyiqk,
                Rcpp::Named("fqkyi") = fqkyi,
                Rcpp::Named("ll") = ll,
                Rcpp::Named("nik") = nik,
                Rcpp::Named("Nik") = Nik,
                Rcpp::Named("probs") = probs,
                Rcpp::Named("ll_individual") = ll_individual
            );
}
///********************************************************************


//    return Rcpp::List::create(
//                Rcpp::Named("rr") = Btheta,
//                Rcpp::Named("rprobs") = rprobs
//            );

// if ( ! R_IsNA( resp(nn,ii) ) ){
//                Rcpp::Rcout << "ii=" << ii << " " <<
//                    ii+resp(nn,ii)*nitems << " " << std::flush << std::endl;


