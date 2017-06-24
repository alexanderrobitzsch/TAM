\name{data.ctest}
\alias{data.ctest}
\alias{data.ctest1}
\alias{data.ctest2}

\docType{data}
\title{
Some C-Test Datasets 
}


\description{
Some C-Test datasets.
}


\usage{
data(data.ctest1)
data(data.ctest2)
}


\format{

\itemize{	
\item The dataset \code{data.ctest1} contains item responses of C-tests
at two time points. The format is

\code{'data.frame':   1675 obs. of  42 variables:} \cr
\code{ $ idstud : num  100101 100102 100103 100104 100105 ...} \cr
\code{ $ idclass: num  1001 1001 1001 1001 1001 ...} \cr
\code{ $ A01T1  : int  0 1 0 1 1 NA 1 0 1 1 ...} \cr
\code{ $ A02T1  : int  0 1 0 1 0 NA 0 1 1 0 ...} \cr
\code{ $ A03T1  : int  0 1 1 1 0 NA 0 1 1 1 ...} \cr
\code{ $ A04T1  : int  1 0 0 0 0 NA 0 0 0 0 ...} \cr
\code{ $ A05T1  : int  0 0 0 1 1 NA 0 0 1 1 ...} \cr
\code{ $ B01T1  : int  1 1 0 1 1 NA 0 0 1 0 ...} \cr
\code{ $ B02T1  : int  0 0 0 1 0 NA 0 0 1 1 ...} \cr
\code{ [...]} \cr
\code{ $ C02T2  : int  0 1 1 1 1 0 1 0 1 1 ...} \cr
\code{ $ C03T2  : int  1 1 0 1 0 0 0 0 1 0 ...} \cr
\code{ $ C04T2  : int  0 0 1 0 0 0 0 1 0 0 ...} \cr
\code{ $ C05T2  : int  0 1 0 0 1 0 1 0 0 1 ...} \cr
\code{ $ D01T2  : int  0 1 1 1 0 1 1 1 1 1 ...} \cr
\code{ $ D02T2  : int  0 1 1 1 1 1 0 1 1 1 ...} \cr
\code{ $ D03T2  : int  1 0 0 0 1 0 0 0 0 0 ...} \cr
\code{ $ D04T2  : int  1 0 1 1 1 0 1 0 1 1 ...} \cr
\code{ $ D05T2  : int  1 0 1 1 1 1 1 1 1 1 ...} \cr

\item The dataset \code{data.ctest2} contains two datasets
(\code{$data1} containing item responses, \code{$data2}
containing sum scores of each C-test) and
a data frame \code{$ITEM} with item informations. 

\code{List of 3} \cr
\code{ $ data1:'data.frame':  933 obs. of  102 variables:} \cr
\code{  ..$ idstud: num [1:933] 10001 10002 10003 10004 10005 ...} \cr
\code{  ..$ female: num [1:933] 1 1 0 0 0 0 1 1 0 1 ...} \cr
\code{  ..$ A101  : int [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  ..$ A102  : int [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  ..$ A103  : int [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  ..$ A104  : int [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  ..$ A105  : int [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  ..$ A106  : int [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  ..$ E115  : int [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  ..$ E116  : int [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  ..$ E117  : int [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  .. [list output truncated]} \cr
\code{ $ data2:'data.frame':  933 obs. of  7 variables:} \cr
\code{  ..$ idstud: num [1:933] 10001 10002 10003 10004 10005 ...} \cr
\code{  ..$ female: num [1:933] 1 1 0 0 0 0 1 1 0 1 ...} \cr
\code{  ..$ A     : num [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  ..$ B     : num [1:933] 16 14 15 13 17 11 11 18 19 13 ...} \cr
\code{  ..$ C     : num [1:933] 17 15 17 14 17 13 9 15 17 12 ...} \cr
\code{  ..$ D     : num [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{  ..$ E     : num [1:933] NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ ITEM :'data.frame':  100 obs. of  3 variables:} \cr
\code{  ..$ item   : chr [1:100] "A101" "A102" "A103" "A104" ...} \cr
\code{  ..$ ctest  : chr [1:100] "A" "A" "A" "A" ...} \cr
\code{  ..$ testlet: int [1:100] 1 1 2 2 2 3 3 3 NA 4 ...} \cr
}	
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
%\examples{
%data(data.ctest1)
%}
\keyword{datasets}
