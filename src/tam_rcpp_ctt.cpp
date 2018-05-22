//// File Name: tam_rcpp_ctt.cpp
//// File Version: 3.19


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

using namespace Rcpp;
using namespace arma;

///********************************************************************
///** tam_rcpp_ctt2
// [[Rcpp::export]]
Rcpp::List tam_rcpp_ctt2( Rcpp::CharacterMatrix TDAT,
    Rcpp::NumericVector WLE, Rcpp::NumericVector MAXK,
    int EST_WLE, Rcpp::NumericVector prg )
{

     int N = TDAT.ncol();
     int I = TDAT.nrow();
     int K = MAXK[0];
     int LP = prg.size();
     int IK = I*K;
     Rcpp::NumericMatrix des( I*K, 9 );
     Rcpp::CharacterVector desV( I*K );

     int start_cc = 0;
     int cc1;
     int pp=0;

     for ( int ii=0; ii<I; ii++){

     // int ii = 0;  // select item ii

     double wles1 = 0;
     double wles2 = 0;
     double wles3 = 0;
     double wles4 = 0;
     double freq = 0;
     double freq2 = 0;
     double mw = 0;

     Rcpp::CharacterVector TDAT_ii = TDAT.row(ii);
     Rcpp::CharacterVector uii = Rcpp::unique( TDAT_ii );

     // Rcpp::IntegerVector uii = table( TDAT_ii );
     int NC_ii = uii.size();

     //Rcpp::CharacterVector categii = uii.attr("names");
     Rcpp::CharacterVector categii = uii;

     for ( int cc=0; cc < NC_ii; cc++ ){
            cc1 = cc + start_cc;
            des( cc1, 0 ) = ii+1;          // item number
            desV[cc1 ] = categii[cc];    // category label
            wles1 = 0;
            wles2 = 0;
            wles3 = 0;
            wles4=0;
            freq = 0;
            freq2 = 0;

         for (int nn=0; nn < N; nn++){
             std::string cx = Rcpp::as<std::string>(TDAT_ii[nn]);
             std::string cy = Rcpp::as<std::string>(categii[cc]);
             if ( cx != "NA" ){
                 if ( cx == cy){
                    if ( EST_WLE==1){
                     wles1 = wles1 + WLE[nn];
                     wles2 = wles2 + WLE[nn]*WLE[nn];
                             }
                     freq ++;
                         } else {
                    if ( EST_WLE==1){
                     wles3 = wles3 + WLE[nn];
                     wles4 = wles4 + WLE[nn]*WLE[nn];
                    }
                     freq2 ++;
                         }
                     }
                 }  // end case nn

        if (cc1> IK){
            Rcpp::stop("Maximum allocation reached! Increase 'allocate'!");
                     }
         des(cc1,3) = freq;    // frequency of students at category cc
         des(cc1,2) = freq2;   // frequency of students not at category cc
         des(cc1,4) = wles1;    // score sum of students at category cc
         des(cc1,5) = wles3;    // score sum of students not at category cc
         des(cc1,6) = wles2;    // sum of squares WLE at category cc
         // calculate N
         des(cc1,1) = des(cc1,2) + des(cc1,3);
         if ( EST_WLE==1){
         // calculate WLE mean total
         mw = ( wles1 + wles3 ) / des( cc1,1);
         // calculate SD total
         des(cc1,8) = sqrt( ( wles2 + wles4 - des(cc1,1)*pow( mw, 2.0 )  )/ ( des(cc1,1) - 1 )  );
         // calculate WLE means
         des(cc1,4) = des(cc1,4) / des(cc1,3);  // M at category cc1
         des(cc1,5) = des(cc1,5) / des(cc1,2);  // M not at category cc1
         // calculate SD of WLE
         des(cc1,6) = sqrt( ( des(cc1,6) - des(cc1,3)*pow( des(cc1,4), 2.0 )  ) / ( des(cc1,3) - 1 ) );
         // calculate point-biserial correlation
         des(cc1,7) =  ( des(cc1,4) - des(cc1,5) )/des(cc1,8) * sqrt( des(cc1,2)*des(cc1,3) /
                         ( des(cc1,1) * ( des(cc1,1)-1 ) ) );
         }
                     }   // end category cc
         start_cc = start_cc + NC_ii;

        // print progress
        if ( ( LP>1) & ( pp<LP) ){
            if ( ii == prg[pp] ){
                    Rcout << "-" << std::flush;
                    pp ++;
//                    R_FlushConsole();
                            }
                    }  // end if progress

                 }    // end item ii

     ///////////////////////////////////////////
     // OUTPUT:
     return Rcpp::List::create(
              Rcpp::Named("des") = des,
              Rcpp::Named("desV") = desV ,
              Rcpp::Named("LP") = LP
                    );
}


///********************************************************************
///** tam_rcpp_ctt3
// [[Rcpp::export]]
Rcpp::List tam_rcpp_ctt3( Rcpp::CharacterMatrix TDAT,
    Rcpp::NumericVector WLE, Rcpp::NumericVector MAXK,
    int EST_WLE, Rcpp::NumericVector prg )
{
    int N = TDAT.ncol();
    int I = TDAT.nrow();
    int K = MAXK[0];
    int LP = prg.size();
    int NC_ii = 0;
    int IK = I*K;
    double eps = 1e-7;
    bool r1;

    Rcpp::NumericMatrix des( I*K, 9 );
    Rcpp::CharacterVector desV( I*K );
    Rcpp::NumericVector wles1 (K);
    Rcpp::NumericVector wles2 (K);
    Rcpp::NumericVector wles3 (K);
    Rcpp::NumericVector wles4 (K);
    Rcpp::NumericVector freq (K);
    Rcpp::NumericVector freq2 (K);
    Rcpp::NumericVector mw (K);
    int start_cc = 0;
    int cc1;
    int pp=0;
    int nn_ii=0;
    double freqall =0;
    double wles3tot=0;
    double wles4tot=0;

    //////////////////////////////////////////////////////////////
    ////////// begin items ///////////////////////////////////////
    for ( int ii=0; ii<I; ii++){
        Rcpp::CharacterVector TDAT_ii = TDAT.row(ii);
        Rcpp::CharacterVector uii = Rcpp::unique( TDAT_ii );
        Rcpp::IntegerVector  uii_match = match( TDAT_ii, uii );
        NC_ii = uii.size();
        if ( NC_ii > K){
            Rcpp::Rcout << "allocate of at least " << NC_ii << "needed!" << std::flush;
            Rcpp::stop("Maximum allocation reached! Increase 'allocate'!");
        }

        // initial values
        for ( int cc=0; cc < NC_ii; cc++ ){
            cc1 = cc + start_cc;
            des( cc1, 0 ) = ii+1; // item number
            desV[cc1 ] = uii[cc];    // category label
            wles1[cc] = 0;
            wles2[cc] = 0;
            wles3tot = 0;
            wles4tot = 0;
            freq[cc] = 0;
        }

        freqall = 0;
        for (int nn=0;nn<N;nn++){
            r1 = ( TDAT_ii[nn] == "NA" );
            if ( ! r1 ){
                nn_ii = uii_match[nn] - 1;
                freq[ nn_ii ] = freq[nn_ii] + 1;
                freqall = freqall + 1;
                if (EST_WLE==1){
                    wles1[ nn_ii ] = wles1[ nn_ii ] + WLE[nn];
                    wles2[ nn_ii ] = wles2[ nn_ii ] + WLE[nn]*WLE[nn];
                    wles3tot = wles3tot + WLE[nn];
                    wles4tot = wles4tot + WLE[nn]*WLE[nn];
                }  // end EST_WLE
            } // end   if (TDAT_ii[nn] != "NA"){
        } // end nn

        for (int cc=0;cc<NC_ii;cc++){
            cc1 = start_cc + cc;
            if (cc1> IK){
                Rcpp::stop("Maximum allocation reached! Increase 'allocate'!");
            }
            des(cc1,3) = freq[cc];    // frequency of students at category cc
            des(cc1,2) = freqall - freq[cc];   // frequency of students not at category cc
            des(cc1,4) = wles1[cc];    // score sum of students at category cc
            des(cc1,5) = wles3tot - wles1[cc];    // score sum of students not at category cc
            des(cc1,6) = wles2[cc];    // sum of squares WLE at category cc
            // calculate N
            des(cc1,1) = des(cc1,2) + des(cc1,3);
            wles3[cc] = wles3tot - wles1[cc];
            wles4[cc] = wles4tot - wles2[cc];
            if ( EST_WLE==1){
                // calculate WLE mean total
                mw[cc] = ( wles1[cc] + wles3[cc] ) / des( cc1,1);
                // calculate SD total
                des(cc1,8) = std::sqrt( ( wles2[cc] + wles4[cc] - des(cc1,1)*pow( mw[cc], 2.0 )  )/ ( des(cc1,1) - 1 + eps)  );
                // calculate WLE means
                des(cc1,4) = des(cc1,4) / des(cc1,3);  // M at category cc1
                des(cc1,5) = des(cc1,5) / des(cc1,2);  // M not at category cc1
                // calculate SD of WLE
                des(cc1,6) = std::sqrt( ( des(cc1,6) - des(cc1,3)*std::pow( des(cc1,4), 2.0 )  ) / ( des(cc1,3) - 1 +eps ) );
                // calculate point-biserial correlation
                des(cc1,7) =  ( des(cc1,4) - des(cc1,5) )/des(cc1,8) * std::sqrt( des(cc1,2)*des(cc1,3) /
                                ( des(cc1,1) * ( des(cc1,1)-1 + eps ) ) );
            }
        }
        // increment start index
        start_cc = start_cc + NC_ii;

        // print progress
        if ( ( LP>1) & ( pp<LP) ){
            if ( ii == prg[pp] ){
            Rcout << "-" << std::flush;
            pp ++;
            //  R_FlushConsole();
        }
        }  // end if progress
    }    // end item ii

    ///////////////////////////////////////////
    // OUTPUT:
    return Rcpp::List::create(
            Rcpp::Named("des") = des,
            Rcpp::Named("desV") = desV ,
            Rcpp::Named("LP") = LP
        );
}

