%% File Name: IRT.residuals.Rd
%% File Version: 0.14

\name{IRT.residuals}
\alias{IRT.residuals}
\alias{IRT.residuals.tam.mml}
\alias{IRT.residuals.tam.mml.2pl}
\alias{IRT.residuals.tam.mml.mfr}
\alias{residuals.tam.mml}
\alias{residuals.tam.mml.2pl}
\alias{residuals.tam.mml.mfr}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Residuals in an IRT Model
}


\description{
Defines an S3 method for the computation of observed residual values.
The computation of residuals is based on weighted likelihood estimates as
person parameters, see \code{\link{tam.wle}}.
\code{IRT.residuals} can only be applied for unidimensional IRT models.
The methods \code{IRT.residuals} and \code{residuals} are equivalent.
}

\usage{
IRT.residuals(object, ...)

\method{IRT.residuals}{tam.mml}(object, ...)
\method{residuals}{tam.mml}(object, ...)

\method{IRT.residuals}{tam.mml.2pl}(object, ...)
\method{residuals}{tam.mml.2pl}(object, ...)

\method{IRT.residuals}{tam.mml.mfr}(object, ...)
\method{residuals}{tam.mml.mfr}(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
Object of class \code{tam.mml}, \code{tam.mml.2pl} or \code{tam.mml.mfr}.
}
\item{\dots}{
Further arguments to be passed
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
List with following entries
\item{residuals}{Residuals}
\item{stand_residuals}{Standardized residuals}
  \item{X_exp}{Expected value of the item response \eqn{X_{pi}}}
  \item{X_var}{Variance of the item response \eqn{X_{pi}}}
  \item{theta}{Used person parameter estimate}
\item{probs}{Expected item response probabilities}   
}
%  \item{xx}{xxx}

%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}

\note{
Residuals can be used to inspect local dependencies in the item
response data, for example using principle component analysis or
factor analysis (see Example 1).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also the \code{eRm::residuals} (\pkg{eRm}) or
\code{\link[mirt:residuals-method]{residuals}} (\pkg{mirt})
functions.

See also \code{\link[TAM:predict.tam.mml]{predict.tam.mml}}.
}


\examples{
#############################################################################
# EXAMPLE 1: Residuals data.read
#############################################################################	

library(sirt)
data(data.read,package="sirt")
dat <- data.read

# for Rasch model
mod <- TAM::tam.mml( dat )
# extract residuals
res <- TAM::IRT.residuals( mod )
str(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Residuals}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
