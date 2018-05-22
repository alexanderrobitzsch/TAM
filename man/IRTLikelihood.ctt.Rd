%% File Name: IRTLikelihood.ctt.Rd
%% File Version: 0.18

\name{IRTLikelihood.ctt}
\alias{IRTLikelihood.ctt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes Individual Likelihood from Classical Test Theory Estimates
}

\description{
Computes individual likelihood from classical test theory estimates
under a unidimensional normal distribution of measurement errors.
}
\usage{
IRTLikelihood.ctt(y, errvar, theta=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Vector of observed scores
}
  \item{errvar}{
Vector of error variances
}
  \item{theta}{
Optional vector for \eqn{\theta} grid.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Object of class \code{IRT.likelihood}
}

%\references{
%% ~put references to the literature/web site here ~
%}

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
# EXAMPLE 1: Individual likelihood and latent regression in CTT
#############################################################################

set.seed(75)

#--- simulate data
N <- 2000
x1 <- stats::rnorm(N)
x2 <- .7 * x1 + stats::runif(N)
# simulate true score
theta <- 1.2 + .6*x1 + .3 *x2 + stats::rnorm(N, sd=sqrt(.50) )
var(theta)
# simulate measurement error variances
errvar <- stats::runif( N, min=.6, max=.9 )
# simulate observed scores
y <- theta + stats::rnorm( N, sd=sqrt( errvar) )

#--- create likelihood object
like1 <- TAM::IRTLikelihood.ctt( y=y, errvar=errvar, theta=NULL )

#--- estimate latent regression
X <- data.frame(x1,x2)
mod1 <- TAM::tam.latreg( like=like1, Y=X )

\dontrun{
#--- draw plausible values
pv1 <- TAM::tam.pv( mod1, normal.approx=TRUE )

#--- create datalist
datlist1 <- TAM::tampv2datalist( pv1, pvnames="thetaPV", Y=X )

#--- statistical inference on plausible values using mitools package
library(mitools)
datlist1a <- mitools::imputationList(datlist1)

# fit linear regression and apply Rubin formulas
mod2 <- with( datlist1a, stats::lm( thetaPV ~ x1 + x2 ) )
summary( mitools::MIcombine(mod2) )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
