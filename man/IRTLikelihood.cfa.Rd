%% File Name: IRTLikelihood.cfa.Rd
%% File Version: 0.16

\name{IRTLikelihood.cfa}
\alias{IRTLikelihood.cfa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Individual Likelihood for Confirmatory Factor Analysis
}
\description{
This function computes the individual likelihood evaluated
at a \code{theta} grid for confirmatory factor analysis
under the normality assumption of residuals. Either
the item parameters (item loadings \code{L}, item
intercepts \code{nu} and residual covariances \code{psi})
or a fitted \code{cfa} object from the \pkg{lavaan}
package can be provided. The individual likelihood
can be used for drawing plausible values.
}

\usage{
IRTLikelihood.cfa(data, cfaobj = NULL, theta = NULL, L = NULL, nu = NULL,
    psi = NULL, snodes = NULL, snodes.adj = 2 , version=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataset with item responses
}
  \item{cfaobj}{
Fitted \code{\link[lavaan:cfa]{lavaan::cfa}} (\pkg{lavaan}) object
}
  \item{theta}{
Optional matrix containing the \code{theta} values
used for evaluating the individual likelihood
}
  \item{L}{
Matrix of item loadings (if \code{cfaobj} is not provided)
}
  \item{nu}{
Vector of item intercepts (if \code{cfaobj} is not provided)
}
  \item{psi}{
Matrix with residual covariances
(if \code{cfaobj} is not provided)
}
  \item{snodes}{
Number of \code{theta} values used for the approximation
of the distribution of latent variables.
}
  \item{snodes.adj}{
Adjustment factor for quasi monte carlo nodes for
more than two latent variables.
}
\item{version}{Function version. \code{version=1} is based on a
    \pkg{Rcpp} implementation while \code{version=0} is
    a pure \R implementation.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Individual likelihood evaluated at \code{theta}
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

\seealso{
\code{\link[CDM:IRT.likelihood]{CDM::IRT.likelihood}}
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Two-dimensional CFA data.Students
#############################################################################

library(lavaan)
library(CDM)

data(data.Students, package="CDM")
dat <- data.Students

dat2 <- dat[ , c(paste0("mj",1:4), paste0("sc",1:4)) ]
# lavaan model with DO operator
lavmodel <- "
DO(1,4,1)
   mj =~ mj\%
   sc =~ sc\%
DOEND
   mj ~~ sc
   mj ~~ 1*mj
   sc ~~ 1*sc
     "
lavmodel <- TAM::lavaanify.IRT( lavmodel , data=dat2 )$lavaan.syntax
cat(lavmodel)

mod4 <- lavaan::cfa( lavmodel , data=dat2 , std.lv=TRUE )
summary(mod4, standardized=TRUE , rsquare=TRUE )
# extract item parameters
res4 <- TAM::cfa.extract.itempars( mod4 )
# create theta grid
theta0 <- seq( -6 , 6 , len=15)
theta <- expand.grid( theta0 , theta0 )
L <- res4$L
nu <- res4$nu
psi <- res4$psi
data <- dat2
# evaluate likelihood using item parameters
like2 <- TAM::IRTLikelihood.cfa( data=dat2 , theta=theta, L=L , nu=nu , psi=psi )
# The likelihood can also be obtained by direct evaluation
# of the fitted cfa object "mod4"
like4 <- TAM::IRTLikelihood.cfa( data=dat2 , cfaobj=mod4 )
attr( like4 , "theta")
# the theta grid is automatically created if theta is not
# supplied as an argument
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
