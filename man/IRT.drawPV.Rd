%% File Name: IRT.drawPV.Rd
%% File Version: 0.13

\name{IRT.drawPV}
\alias{IRT.drawPV}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Function for Drawing Plausible Values
}

\description{
This function draws plausible values of a continuous latent variable
based on a fitted object for which the
\code{\link[CDM:IRT.posterior]{CDM::IRT.posterior}} method
is defined.
}

\usage{
IRT.drawPV(object,NPV=5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object for which the method \code{\link[CDM:IRT.posterior]{CDM::IRT.posterior}}
  does exist.}
\item{NPV}{Number of plausible values to be drawn.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Matrix with plausible values
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
%See \code{\link{IRTLikelihood.cfa}} for extracting the
%}



\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Plausible value imputation for Rasch model in sirt
#############################################################################

library(sirt)
data(data.read,package="sirt")
dat <- data.read

# fit Rasch model
mod <- rasch.mml2(dat)
# draw 10 plausible values
pv1 <- TAM::IRT.drawPV(mod, NPV=10)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Plausible value imputation}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
