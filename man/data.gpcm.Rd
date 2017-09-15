%% File Name: data.gpcm.Rd
%% File Version: 2.08
%% File Last Change: 2017-06-22 17:41:51

\name{data.gpcm}
\alias{data.gpcm}
\docType{data}
\title{
Dataset with Ordered Indicators
}
\description{
Dataset with ordered values of 3 indicators
}
\usage{data(data.gpcm)}
\format{
  A data frame with 392 observations on the following 3 items.
  \describe{
    \item{\code{Comfort}}{a numeric vector}
    \item{\code{Work}}{a numeric vector}
    \item{\code{Benefit}}{a numeric vector}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
The dataset is copied from the \pkg{ltm} package.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(data.gpcm)
summary(data.gpcm)
}
\keyword{datasets}
