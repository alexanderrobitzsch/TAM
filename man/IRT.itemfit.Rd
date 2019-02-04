%% File Name: IRT.itemfit.Rd
%% File Version: 0.23

\name{IRT.itemfit.tam}

\alias{IRT.itemfit.tam.mml}
\alias{IRT.itemfit.tam.mml.2pl}
\alias{IRT.itemfit.tam.mml.mfr}
\alias{IRT.itemfit.tam.mml.3pl}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
RMSD Item Fit Statistics for \pkg{TAM} Objects
}

\description{
Computes the RMSD item fit statistic (formerly labeled as RMSEA;
Yamamoto, Khorramdel, & von Davier, 2013) for fitted objects in the
\pkg{TAM} package, see
\code{\link[CDM:IRT.itemfit]{CDM::IRT.itemfit}} and
\code{\link[CDM:IRT.RMSD]{CDM::IRT.RMSD}}.
}




\usage{
\method{IRT.itemfit}{tam.mml}(object, method="RMSD", \dots)

\method{IRT.itemfit}{tam.mml.2pl}(object, method="RMSD", \dots)

\method{IRT.itemfit}{tam.mml.mfr}(object, method="RMSD", \dots)

\method{IRT.itemfit}{tam.mml.3pl}(object, method="RMSD", \dots)
}



%- maybe also 'usage' for other objects documented here.

\arguments{
\item{object}{
Object of class \code{tam.mml}, \code{tam.mml.2pl}, \code{tam.mml.mfr}
or \code{tam.mml.3pl}.
}
\item{method}{Requested method for item fit calculation. Currently,
only the RMSD fit statistic (formerly labeled as the RMSEA statistic,
see \code{\link[CDM:IRT.RMSD]{CDM::IRT.RMSD}})
can be used.
}
\item{\dots}{
Further arguments to be passed.
}
}

%\item{type}{Type of person parameter estimate. \code{"PV"} (plausible values),
%\code{"WLE"} (weighted likelihood estimates) and
%\code{"Pop"} (population trait distribution) can be specified.
%}

%\details{
%A Wright map is only created for models with an assumed normal distibution.
%Hence, not for all models of the \code{\link{tamaan}} functions
%Wright maps are created.
%}


%\value{
%}



\references{
Yamamoto, K., Khorramdel, L., & von Davier, M. (2013). Scaling PIAAC cognitive data.
In OECD (Eds.). \emph{Technical Report of the Survey of Adults Skills (PIAAC)} (Ch. 17).
Paris: OECD.
}


%\note{
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[CDM:IRT.itemfit]{CDM::IRT.itemfit}},
\code{\link[CDM:IRT.RMSD]{CDM::IRT.RMSD}}
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: RMSD item fit statistic data.read
#############################################################################

library(sirt)
data(data.read,package="sirt")
dat <- data.read

#*** fit 1PL model
mod1 <- TAM::tam.mml( dat )
summary(mod1)

#*** fit 2PL model
mod2 <- TAM::tam.mml.2pl( dat )
summary(mod2)

#*** assess RMSEA item fit
fmod1 <- IRT.itemfit(mod1)
fmod2 <- IRT.itemfit(mod2)
# summary of fit statistics
summary( fmod1 )
summary( fmod2 )

#############################################################################
# EXAMPLE 2: Simulated 2PL data and fit of 1PL model
#############################################################################

set.seed(987)
N <- 1000    # 1000 persons
I <- 10      # 10 items
# define item difficulties and item slopes
b <- seq(-2,2,len=I)
a <- rep(1,I)
a[c(3,8)] <- c( 1.7, .4 )
# simulate 2PL data
dat <- sirt::sim.raschtype( theta=rnorm(N), b=b, fixed.a=a)

# fit 1PL model
mod <- TAM::tam.mml( dat )

# RMSEA item fit
fmod <- IRT.itemfit(mod)
round( fmod, 3 )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{Wright map}
%% \keyword{Thurstonian thresholds}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
