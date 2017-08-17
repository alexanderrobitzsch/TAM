\name{tam_irf_3pl}
\alias{tam_irf_3pl}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Item Response Function for the 3PL Model
}

\description{
Computes the item response function for the 3PL model in the \pkg{TAM}
package.
}

\usage{
tam_irf_3pl(theta, AXsi, B, guess=NULL, subtract_max=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{theta}{
Matrix or vector of \eqn{\bold{\theta}} values
}
\item{AXsi}{Matrix of item-category parameters}
\item{B}{Array containing item-category loadings} 
\item{guess}{Optional parameter of guessing parameters}
\item{subtract_max}{Logical indicating whether numerical underflow in 
probabilities should be explicitly avoided}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Array containing item response probabilities arranged by the dimensions
theta points \eqn{\times} items \eqn{\times} categories 
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

% \seealso{
% xxxx
% }


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: 2PL example
#############################################################################

library(sirt)
data(data.read, package="sirt")
dat <- data.read

#* estimate 2PL model
mod <- TAM::tam.mml.2pl( resp = dat )
#* define theta vector
theta <- seq(-3,3, len=41)
#* compute item response probabilities
probs <- TAM::tam_irf_3pl( theta=theta , AXsi = mod$AXsi , B = mod$B )
str(probs)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{DO statements}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
