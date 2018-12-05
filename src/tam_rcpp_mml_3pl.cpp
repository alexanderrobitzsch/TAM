//// File Name: tam_rcpp_mml_3pl.cpp
//// File Version: 3.39


#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** tam_rcpp_mml_3pl_calc_Fdes
// [[Rcpp::export]]
Rcpp::List tam_rcpp_mml_3pl_calc_Fdes( Rcpp::NumericVector XDES, Rcpp::NumericVector dimXdes )
{
    int I= dimXdes[0];
    int maxK= dimXdes[1];
    int TP= dimXdes[2];
    int Nlam = dimXdes[3];
    int RR = XDES.size();
    Rcpp::NumericMatrix XdesM(RR,5);
    int rr = 0;
    int ind = 0;
    for (int ii=0; ii<I;ii++){
        for (int kk=0; kk <maxK; kk++){
            for ( int tt=0; tt<TP; tt++ ){
                for ( int ll=0; ll<Nlam; ll++ ){
                    rr=ii+I*kk+I*maxK*tt+I*maxK*TP*ll;
                    if ( XDES[rr] != 0 ){
                        XdesM(ind,0) = ii;
                        XdesM(ind,1) = kk;
                        XdesM(ind,2) = tt;
                        XdesM(ind,3) = ll;
                        XdesM(ind,4) = XDES[rr];
                        ind = ind + 1;
                    }
                }
            }
        }
    }
    //---- OUTPUT
    return Rcpp::List::create(
            Rcpp::Named("NFdesM") = ind,
            Rcpp::Named("FdesM") = XdesM
        );
}
///********************************************************************

// Rcpp::Rcout << "rr = " << rr << " XDES[rr] = " << XDES[rr] << std::endl;

///********************************************************************
///** tam_rcpp_mml_3pl_slca_deriv
// [[Rcpp::export]]
Rcpp::List tam_rcpp_mml_3pl_slca_deriv( Rcpp::NumericMatrix XdesM,
    Rcpp::NumericVector dimXdes, Rcpp::NumericVector Xlambda,
    Rcpp::NumericVector probs, Rcpp::NumericVector nik, Rcpp::NumericVector Nik,
    Rcpp::NumericVector guess, Rcpp::NumericVector probs0 )
{
    int I= dimXdes[0];
    int maxK= dimXdes[1];
    int TP= dimXdes[2];
    int Nlam = dimXdes[3];
    int RR = XdesM.nrow();
    double eps10 = 1E-7;
    Rcpp::NumericVector d1b(Nlam);
    Rcpp::NumericVector d2b(Nlam);
    int ii=0;
    int hh=0;
    int tt=0;
    int ll=0;

    //  # XdesM[ii,kk,tt,ll, value ]
    //   # probs  num [1:I, 1:maxK, 1:TP]
    //   # n.ik  num [1:I, 1:maxK, 1:TP]
    //   # N.ik  num [1:I,1:TP]
    //   # Xdes  num [1:I, 1:maxK, 1:TP, 1:Nlam]
    ///*********************
    // First derivative

    //  for (hh in 1:maxK){
    // t1 <- sum( Xdes[, hh,, ll] * ( n.ik[, hh, ] - probs[,hh,] * N.ik ) )
    //  d1.b[ll] <- d1.b[ll] + t1
    //            }
    for (int rr = 0; rr <RR; rr++){
        // # XdesM[ii,kk,tt,ll, value ]
        ii = XdesM(rr,0);  // item ii
        hh = XdesM(rr,1);  // category hh
        tt = XdesM(rr,2);  // theta grid point tt
        ll = XdesM(rr,3);  // gamma parameter ll
        //*** no guessing parameter
        if ( guess[ii] <= eps10 ){
            d1b[ll] += XdesM(rr,4) * ( nik[ii+I*hh+I*maxK*tt] -
            probs[ii+I*hh+I*maxK*tt] * Nik[ ii+I*tt ] );
        } // end if guess[ii] = 0
        //*** with guessing parameter
        if ( guess[ii] > eps10 ){
            if (hh==1 ){
                d1b[ll] += XdesM(rr,4) * probs0[ii+I*hh+I*maxK*tt] / probs[ii+I*hh+I*maxK*tt] *
                            ( nik[ii+I*hh+I*maxK*tt] - probs[ii+I*hh+I*maxK*tt] * Nik[ ii+I*tt ] );
            }
        }  // end guess[ii] > 0
    }

    ///*********************
    // Second derivative

    int NS = I*TP*Nlam;
    Rcpp::NumericVector tmp1(NS);
    // tmp1 <- 0
    // for (hh in 1:maxK ){
    //   tmp1 <- tmp1 + probs[,hh,] * Xdes[,hh,,ll]
    //        }

    // parameter ll; item i; class tt
    int vv=0;
    for (int rr=0;rr<RR;rr++){
        ii = XdesM(rr,0);
        hh = XdesM(rr,1);
        tt = XdesM(rr,2);
        ll = XdesM(rr,3);
        vv = ii + I*tt + I*TP*ll;
        tmp1[vv] += XdesM(rr,4) * probs[ii+I*hh+I*maxK*tt];
    }

    // for (hh in 1:maxK){
    //  t2 <- sum( Xdes[, hh,, ll] * N.ik * probs[,hh,] *
    //   ( Xdes[, hh,, ll ] - tmp1 ) )
    // d2.b[ll] <- d2.b[ll] + t2
    //        }

    // int rr = 0;
    for (int rr=0;rr<RR;rr++){
        ii = XdesM(rr,0);
        hh = XdesM(rr,1);
        tt = XdesM(rr,2);
        ll = XdesM(rr,3);
        vv = ii + I*tt + I*TP*ll;
        //  t2 <- sum( Xdes[, hh,, ll] * N.ik * probs[,hh,] *
        //   ( Xdes[, hh,, ll ] - tmp1 ) )
        if ( guess[ii] <= eps10 ){
            d2b[ll] += XdesM(rr,4) * Nik[ii + I*tt ] * probs[ ii + I*hh + I*maxK*tt ] *
                        ( XdesM(rr,4) - tmp1[vv] );
        }  // end if guess[ii] = 0
        if ( guess[ii] >= eps10 ){
            if (hh==1){
                d2b[ll] += std::pow(XdesM(rr,4), 2.0) * probs0[ ii + I*hh + I*maxK*tt ] *
                            probs0[ ii + I*0 + I*maxK*tt ] * ( guess[ii] * nik[ii+I*hh+I*maxK*tt] /
                            std::pow( probs[ ii + I*hh + I*maxK*tt ], 2.0) - Nik[ii + I*tt ] );
            }
        }  // end if guess[ii] > 0
    }
    //---- OUTPUT
    return Rcpp::List::create(
            Rcpp::Named("d1b") = d1b,
            Rcpp::Named("d2b") = d2b
        );
}
///********************************************************************

///********************************************************************
///** tam_rcpp_mml_3pl_calcexp
// [[Rcpp::export]]
Rcpp::List tam_rcpp_mml_3pl_calcexp( int NP,
    Rcpp::NumericMatrix rprobs, Rcpp::NumericMatrix A,
    Rcpp::NumericMatrix INDEXIPNO, Rcpp::NumericVector INDEXIPLIST2,
    Rcpp::NumericVector ESTXSIINDEX, int C,
    Rcpp::NumericMatrix ITEMWT, Rcpp::NumericMatrix rprobs0,
    Rcpp::NumericVector GUESS, Rcpp::NumericVector nik,
    Rcpp::NumericVector ni )
{
    ////////////////////////////////////////////////////////////
    // define output vectors
    Rcpp::NumericVector XBAR (NP);
    Rcpp::NumericVector iscore (NP);
    Rcpp::NumericVector XBAR2 (NP);
    Rcpp::NumericVector XXF (NP);
    ///////////////////////////////////////////////////////////
    // DEFINE indices
    int TP = rprobs.ncol();
    int NEXI = ESTXSIINDEX.size();
    int NR = rprobs.nrow();
    int II = NR / C;
    // II ... number of items
    // TP ... number of theta points
    // CC ... number of categories

    /////////////////////////////////////////////////////////
    // CALCULATIONS

    // loop over xsi item parameters
    for (int hh=0;hh<NEXI;++hh){
        double pp = ESTXSIINDEX[hh] - 1;
        double ll = 0; // xbar element
        double yy = 0; // xbar2 element
        double zz = 0; // xxf element
        double ww = 0; // iscore element

        // loop over theta points
        for (int tt=0;tt<TP;++tt){
            // loop over items within an item parameter group
            double GG = INDEXIPNO(pp,1) - INDEXIPNO(pp,0) + 1;
            for (int gg=0;gg<GG;++gg){
                double ii=INDEXIPLIST2[ INDEXIPNO(pp,0)-1 + gg ]-1;
                // loop over categories cc = 1, ..., C
                double vv1 = 0;  // xbar counter
                double vv2 = 0;  // xxf counter
                double vv3 = 0;
                for (int cc=0;cc<C;++cc){
                    if ( GUESS[ii] == 0 ){
                        // xbar calculation
                        vv1 += A( ii+cc*II, pp ) * rprobs( ii + cc*II, tt );
                        vv2 += A( ii+cc*II, pp) * A( ii+cc*II, pp) * rprobs( ii + cc*II, tt );
                        vv3 += A( ii+cc*II, pp) * nik[ ii +cc*II + tt*II*C ];
                    }  // end guess[ii] == 0
                    if ( ( GUESS[ii] > 0 ) & ( cc == 1 ) ){
                        // xbar calculation
                        vv1 += A( ii+cc*II, pp ) * rprobs0( ii + cc*II, tt );
                        vv2 += A( ii+cc*II, pp) * A( ii+cc*II, pp) * rprobs0( ii + cc*II, tt ) *
                            rprobs0( ii, tt ) * ( GUESS[ii] / std::pow(rprobs( ii + cc*II, tt ),2.0) *
                                nik[ ii +cc*II + tt*II*C ] - ni[ii+tt*II] );
                        vv3 += A( ii+cc*II, pp) * nik[ ii +cc*II + tt*II*C ] *
                                rprobs0( ii + cc*II, tt ) / rprobs( ii + cc*II, tt );
                    }   // end guess[ii] > 0
                }  // end categories cc
                if ( GUESS[ii] == 0 ){
                    ll += vv1*ni[ii+tt*II];
                    yy += vv1*vv1*ni[ii+tt*II]; // xbar2 addition
                    zz += -vv2*ni[ii+tt*II]; // xxf addition
                    ww += vv3;
                } // end guess ii
                if ( GUESS[ii] > 0 ){
                    ll += vv1*ni[ii+tt*II];
                    zz += vv2;
                    ww += vv3;
                } // end guess ii
            }   // end item parameter group gg
        }    // end theta grid tt
        XBAR[pp] = ll;
        iscore[pp] = ww;
        XBAR2[pp] = yy;
        XXF[pp] = zz;
    } // end xsi index

    ///////////////////////////////////////////////////////
    ///////////// O U T P U T   ///////////////////////////
    return Rcpp::List::create(
            Rcpp::Named("xbar")=XBAR,
            Rcpp::Named("xbar2")=XBAR2,
            Rcpp::Named("xxf")=XXF,
            Rcpp::Named("iscore") = iscore
        );
}
///********************************************************************

///********************************************************************
///** tam_rcpp_mml_3pl_compute_B
// [[Rcpp::export]]
Rcpp::List tam_rcpp_mml_3pl_compute_B( Rcpp::NumericMatrix Edes,
    Rcpp::NumericVector gammaslope, Rcpp::NumericVector dimE )
{
    int NE=Edes.nrow();
    int I=dimE[0];
    int K=dimE[1];
    int D=dimE[2];
    Rcpp::NumericVector B(I*K*D);
    int ii=0;
    int kk=0;
    int dd=0;
    int ll=0;
    int ind=0;

    for (int lz=0;lz<NE;lz++){
        ii = Edes(lz,0);
        kk = Edes(lz,1);
        dd = Edes(lz,2);
        ll = Edes(lz,3);
        // index Edes(,4); value Edes(,5);
        ind = ii + I*kk + I*K*dd;
        B[ind] += Edes(lz,5)*gammaslope[ll];
        // ind = ii + I*kk + I*K*dd + I*K*D*ll;
    }
    //----- OUTPUT
    return Rcpp::List::create(
            Rcpp::Named("E_design") = Edes,
            Rcpp::Named("B") = B
        );
}
///********************************************************************

///********************************************************************
///** tam_rcpp_mml_3pl_nonzero_entries
// [[Rcpp::export]]
Rcpp::List tam_rcpp_mml_3pl_nonzero_entries( Rcpp::NumericVector E,
    Rcpp::NumericVector dimE )
{
    int NE = E.size();
    int I=dimE[0];
    int K=dimE[1];
    int D=dimE[2];
    int L=dimE[3];
    Rcpp::NumericMatrix E_design(NE,6);
    int lz=0;
    int ind=0;

    for (int ll=0;ll<L;ll++){
        for (int dd=0;dd<D;dd++){
            for (int kk=0;kk<K;kk++){
                for (int ii=0;ii<I;ii++){
                    ind = ii + I*kk + I*K*dd + I*K*D*ll;
                    if ( E[ind] != 0 ){
                        E_design(lz,0) = ii;
                        E_design(lz,1) = kk;
                        E_design(lz,2) = dd;
                        E_design(lz,3) = ll;
                        E_design(lz,4) = ind;
                        E_design(lz,5) = E[ind];
                        lz ++;
                    }
                }  // end ii
            }  // end kk
        } // end dd
    } // end ll

    E_design = E_design( Rcpp::Range(0,lz-1), Rcpp::Range(0,5) );

    //----- OUTPUT
    return Rcpp::List::create(
            Rcpp::Named("E_design") = E_design,
            Rcpp::Named("maxE") = lz
        );
}
///********************************************************************
