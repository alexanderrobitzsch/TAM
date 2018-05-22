%% File Name: WLErel.Rd
%% File Version: 0.20

\name{WLErel}
\alias{WLErel}
\alias{EAPrel}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Reliability Estimation in \pkg{TAM}
}

\description{
Functions for computing reliability estimates.
}

\usage{
WLErel(theta, error, w=rep(1, length(theta)), select=NULL)

EAPrel(theta, error, w=rep(1, length(theta)), select=NULL)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
Vector with theta estimates
}
\item{error}{
Vector with standard errors of theta estimates
}
  \item{w}{
Optional vector of person weights
}
\item{select}{Optional vector for selecting cases}
}

\details{
The reliability formulas follow Adams (2005). Let \eqn{v} denote
the variance of \code{theta} estimates and let \eqn{s} denote
the average of the squared \code{error}. Then, the WLE reliability is
defined as \eqn{1-s/v=(v-s)/v} while the EAP reliability is defined as
\eqn{1 - s/(s+v)=v/(s+v)}.
}


\value{
Numeric value
}

\references{
Adams, R. J. (2005). Reliability as a measurement design effect.
\emph{Studies in Educational Evaluation, 31}(2), 162-172.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}


\examples{
#############################################################################
# EXAMPLE 1: Toy example for reliability functions
#############################################################################

set.seed(9897)
N <- 100
# simulate theta and error SDs
x <- stats::rnorm(N,sd=2)
error <- stats::runif(N, .7, 1.3)
# compute WLE reliability
WLErel(x,error)
# compute EAP reliaility
EAPrel(x,error)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
