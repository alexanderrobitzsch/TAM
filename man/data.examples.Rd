%% File Name: data.examples.Rd
%% File Version: 2.17

\name{data.examples}


\alias{data.ex08}
\alias{data.ex10}
\alias{data.ex11}
\alias{data.ex12}
\alias{data.ex14}
\alias{data.ex15}
\alias{data.exJ03}
\alias{data.ex16}
\alias{data.ex17}


\docType{data}

\title{
Datasets \code{data.ex} in \pkg{TAM} Package
}
\description{
Datasets included in the \pkg{TAM} package
}
\usage{
data(data.ex08)
data(data.ex10)
data(data.ex11)
data(data.ex12)
data(data.ex14)
data(data.ex15)
data(data.exJ03)
}

\format{
\itemize{
%% Example data.ex08
\item Data \code{data.ex08} for Example 8 in \code{\link{tam.mml}}
has the following format:

\code{List of 2} \cr
\code{ $ facets:'data.frame': 1000 obs. of  1 variable:} \cr
\code{  ..$ female: int [1:1000] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ resp  : num [1:1000, 1:10] 1 1 1 0 1 0 1 1 0 1 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:10] "I0001" "I0002" "I0003" "I0004" ...} \cr

\item Data \code{data.ex10} for Example 10 in \code{\link{tam.mml}}
has the following format:

\code{'data.frame':   675 obs. of  7 variables:} \cr
\code{ $ pid  : int  1 1 1 2 2 3 3 4 4 5 ...} \cr
\code{ $ rater: int  1 2 3 2 3 1 2 1 3 1 ...} \cr
\code{ $ I0001: num  0 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ I0002: num  1 1 1 1 1 0 1 1 1 1 ...} \cr
\code{ $ I0003: num  1 1 1 1 0 0 0 1 0 1 ...} \cr
\code{ $ I0004: num  0 1 0 0 1 0 1 0 1 0 ...} \cr
\code{ $ I0005: num  0 0 1 1 1 0 0 1 0 1 ...} \cr

% Example 11
\item Data \code{data.ex11} for Example 11 in \code{\link{tam.mml}}
has the following format:

\code{'data.frame':   3400 obs. of  13 variables:} \cr
\code{ $ booklet: chr  "B1" "B1" "B3" "B2" ...} \cr
\code{ $ M133   : int  1 1 NA 1 NA 1 NA 1 0 1 ...} \cr
\code{ $ M176   : int  1 0 1 NA 0 0 0 NA NA NA ...} \cr
\code{ $ M202   : int  NA NA NA 0 NA NA NA 0 0 0 ...} \cr
\code{ $ M212   : int  NA NA 1 0 0 NA 0 1 0 0 ...} \cr
\code{ $ M214   : int  1 0 1 1 0 0 0 0 1 0 ...} \cr
\code{ $ M259   : int  NA NA 1 1 1 NA 1 1 1 1 ...} \cr
\code{ $ M303   : int  NA NA 1 1 1 NA 1 1 1 0 ...} \cr
\code{ $ M353   : int  NA NA NA 1 NA NA NA 1 1 9 ...} \cr
\code{ $ M355   : int  NA NA NA 1 NA NA NA 1 1 0 ...} \cr
\code{ $ M444   : int  0 0 0 NA 0 0 0 NA NA NA ...} \cr
\code{ $ M446   : int  1 0 0 1 0 1 1 1 0 0 ...} \cr
\code{ $ M449   : int  NA NA NA 1 NA NA NA 1 1 1 ...}

Missing responses by design are coded as \code{NA}, omitted
responses are coded as \code{9}. \cr

% Example 12
\item Data \code{data.ex12} for Example 12 in \code{\link{tam.mml}}
has the following format:

\code{ num [1:100, 1:10] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ - attr(*, "dimnames")=List of 2} \cr
\code{  ..$ : NULL} \cr
\code{  ..$ : chr [1:10] "I0001" "I0002" "I0003" "I0004" ...} \cr

% Example 14
\item Data \code{data.ex14} for Example 14 in \code{\link{tam.mml}}
has the following format:

\code{'data.frame':   1110 obs. of  11 variables:} \cr
\code{ $ pid  : num  1001 1001 1001 1001 1001 ...} \cr
\code{ $ X1   : num  1 1 1 1 1 1 0 0 0 0 ...} \cr
\code{ $ X2   : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ rater: int  4 4 4 4 4 4 4 4 4 4 ...} \cr
\code{ $ crit1: int  0 0 2 1 1 2 0 0 0 0 ...} \cr
\code{ $ crit2: int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ crit3: int  0 1 1 0 0 1 0 0 1 0 ...} \cr
\code{ $ crit4: int  0 0 0 1 0 0 0 0 0 0 ...} \cr
\code{ $ crit5: int  0 0 0 0 1 1 0 0 0 0 ...} \cr
\code{ $ crit6: int  0 0 0 0 1 0 0 0 0 0 ...} \cr
\code{ $ crit7: int  1 0 2 0 0 0 0 0 0 0 ...} \cr

% Example 15
\item Data \code{data.ex15} for Example 15 in \code{\link{tam.mml}}
has the following format:

\code{'data.frame':   2155 obs. of  182 variables:} \cr
\code{ $ pid    : num  10001 10002 10003 10004 10005 ...} \cr
\code{ $ group  : num  1 1 0 0 1 0 1 0 1 1 ...} \cr
\code{ $ Item001: num  0 NA NA 0 NA NA NA 0 0 NA ...} \cr
\code{ $ Item002: num  1 NA NA 1 NA NA NA NA 1 NA ...} \cr
\code{ $ Item003: num  NA NA NA NA 1 NA NA NA NA 1 ...} \cr
\code{ $ Item004: num  NA NA 0 NA NA NA NA NA NA NA ...} \cr
\code{ $ Item005: num  NA NA 1 NA NA NA NA NA NA NA ...} \cr
\code{[...]}

This dataset shows an atypical convergence behavior. Look at Example 15
to fix convergence problems using arguments \code{increment.factor} and
\code{fac.oldxsi}. \cr

%% Example data.exJ03
\item Data \code{data.exJ03} for Example 4 in \code{\link{tam.jml}}
has the following format:

\code{List of 2} \cr
\code{ $ resp:'data.frame':   40 obs. of  20 variables:} \cr
\code{  ..$ I104: int [1:40] 4 5 6 5 3 4 3 5 4 6 ...} \cr
\code{  ..$ I118: int [1:40] 6 4 6 5 3 2 5 3 5 4 ...} \cr
\code{[...]}
\code{  ..$ I326: int [1:40] 6 1 5 1 4 2 4 1 6 1 ...} \cr
\code{  ..$ I338: int [1:40] 6 2 6 1 6 2 4 1 6 1 ...} \cr
\code{ $ X   :'data.frame':   40 obs. of  4 variables:} \cr
\code{  ..$ rater : int [1:40] 40 40 96 96 123 123 157 157 164 164 ...} \cr
\code{  ..$ gender: int [1:40] 2 2 1 1 1 1 2 2 2 2 ...} \cr
\code{  ..$ region: num [1:40] 1 1 1 1 2 2 1 1 1 1 ...} \cr
\code{  ..$ leader: int [1:40] 1 2 1 2 1 2 1 2 1 2 ...}

It is a rating dataset (a subset of a dataset provided by Matt Barney). \cr

%% Example data.ex16

\item Data \code{data.ex16} contains dichotomous item response data from
three studies corresponding to three grades.

\code{'data.frame':   3235 obs. of  25 variables:} \cr
\code{ $ idstud: num  1e+05 1e+05 1e+05 1e+05 1e+05 ...} \cr
\code{ $ grade : num  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ A1    : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ B1    : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ C1    : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ D1    : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ E1    : int  0 0 1 0 1 1 1 0 1 1 ...} \cr
\code{ $ E2    : int  1 1 1 1 1 1 1 0 1 1 ...} \cr
\code{ $ E3    : int  1 1 1 1 1 1 1 0 1 1 ...} \cr
\code{ $ F1    : int  1 0 1 1 0 0 1 0 1 1 ...} \cr
\code{ $ G1    : int  0 1 1 1 1 0 1 0 1 1 ...} \cr
\code{ $ G2    : int  1 1 1 1 1 0 0 0 1 1 ...} \cr
\code{ $ G3    : int  1 0 1 1 1 0 0 0 1 1 ...} \cr
\code{ $ H1    : int  1 0 1 1 1 0 0 0 1 1 ...} \cr
\code{ $ H2    : int  1 0 1 1 1 0 0 0 1 1 ...} \cr
\code{ $ I1    : int  1 0 1 0 1 0 0 0 1 1 ...} \cr
\code{ $ I2    : int  1 0 1 0 1 0 0 0 1 1 ...} \cr
\code{ $ J1    : int  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ K1    : int  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ L1    : int  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ L2    : int  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ L3    : int  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ M1    : int  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ M2    : int  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ M3    : int  NA NA NA NA NA NA NA NA NA NA ...} \cr


%% Example data.ex17

\item Data \code{data.ex17} contains polytomous item response data from
three studies corresponding to three grades.

\code{'data.frame':   3235 obs. of  15 variables:} \cr
\code{ $ idstud: num  1e+05 1e+05 1e+05 1e+05 1e+05 ...} \cr
\code{ $ grade : num  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ A     : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ B     : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ C     : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ D     : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ E     : num  2 2 3 2 3 3 3 0 3 3 ...} \cr
\code{ $ F     : int  1 0 1 1 0 0 1 0 1 1 ...} \cr
\code{ $ G     : num  2 2 3 3 3 0 1 0 3 3 ...} \cr
\code{ $ H     : num  2 0 2 2 2 0 0 0 2 2 ...} \cr
\code{ $ I     : num  2 0 2 0 2 0 0 0 2 2 ...} \cr
\code{ $ J     : int  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ K     : int  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ L     : num  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ M     : num  NA NA NA NA NA NA NA NA NA NA ...} \cr

} %% end itemize
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%The dataset is copied from the \pkg{ltm} package.
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\seealso{
These examples are used in the \code{\link{tam.mml}} Examples.
    }
%\examples{
%data(data.gpcm)
%summary(data.gpcm)
%}
\keyword{datasets}
