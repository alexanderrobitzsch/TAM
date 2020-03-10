%% File Name: IRT.informationCurves.Rd
%% File Version: 0.211

\name{IRT.informationCurves}
\alias{IRT.informationCurves}
\alias{IRT.informationCurves.tam.mml}
\alias{IRT.informationCurves.tam.mml.2pl}
\alias{IRT.informationCurves.tam.mml.3pl}
\alias{IRT.informationCurves.tam.mml.mfr}
\alias{plot.IRT.informationCurves}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Item and Test Information Curve
}

\description{
An S3 method which computes item and test information curves, see Muraki (1993).
}

\usage{
IRT.informationCurves(object, ...)

\method{IRT.informationCurves}{tam.mml}( object, h=.0001, iIndex=NULL,
          theta=NULL, ... )

\method{IRT.informationCurves}{tam.mml.2pl}( object, h=.0001, iIndex=NULL,
          theta=NULL, ... )

\method{IRT.informationCurves}{tam.mml.mfr}( object, h=.0001, iIndex=NULL,
          theta=NULL, ... )

\method{IRT.informationCurves}{tam.mml.3pl}( object, h=.0001, iIndex=NULL,
          theta=NULL, ... )

\method{plot}{IRT.informationCurves}(x, curve_type="test", \dots)
}

% informationCurves_mml <- function( object, h=.0001,
%    iIndex=NULL, theta=NULL, ... )

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{tam.mml}, \code{tam.mml.2pl}, \code{tam.mml.mfr}
or \code{tam.mml.3pl}.
}
  \item{\dots}{
Further arguments to be passed
}
\item{h}{Numerical differentiation parameter}
\item{iIndex}{Indices of items for which test information should be computed.
The default is to use all items.}
\item{theta}{Optional vector of \eqn{\theta} for which information curves
should be computed.}
\item{curve_type}{Type of information to be plotted. It can be \code{"test"}
    for the test information curve and \code{"se"} for the
standard error curve.}
  \item{x}{
Object of class \code{tam.mml}, \code{tam.mml.2pl}, \code{tam.mml.mfr}
or \code{tam.mml.3pl}.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List with following entries
  \item{se_curve}{Standard error curves}
  \item{test_info_curve}{Test information curve}
  \item{info_curves_item}{Item information curves}
  \item{info_curves_categories}{Item-category information curves}
  \item{theta}{Used \eqn{\theta} grid}
}

\references{
Muraki, E. (1993). Information functions of the generalized partial credit
model. \emph{Applied Psychological Measurement, 17}(4), 351-363.
\doi{10.1177/014662169301700403}
}
%\author{
%Alexander Robitzsch
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Dichotomous data | data.read
#############################################################################

data(data.read, package="sirt")
dat <- data.read

# fit 2PL model
mod1 <- TAM::tam.mml.2pl( dat )
summary(mod1)

# compute information curves at grid seq(-5,5,length=100)
imod1 <- TAM::IRT.informationCurves( mod1, theta=seq(-5,5,len=100)  )
str(imod1)
# plot test information
plot( imod1 )
# plot standard error curve
plot( imod1, curve_type="se", xlim=c(-3,2) )
# cutomized plot
plot( imod1, curve_type="se", xlim=c(-3,2), ylim=c(0,2), lwd=2, lty=3)

#############################################################################
# EXAMPLE 2: Mixed dichotomous and polytomous data
#############################################################################

data(data.timssAusTwn.scored, package="TAM")
dat <- data.timssAusTwn.scored
# select item response data
items <- grep( "M0", colnames(dat), value=TRUE )
resp <- dat[, items ]

#*** Model 1: Partial credit model
mod1 <- TAM::tam.mml( resp )
summary(mod1)
# information curves
imod1 <- TAM::IRT.informationCurves( mod1, theta=seq(-3,3,len=20)  )

#*** Model 2: Generalized partial credit model
mod2 <- TAM::tam.mml.2pl( resp, irtmodel="GPCM")
summary(mod2)
imod2 <- TAM::IRT.informationCurves( mod2 )

#*** Model 3: Mixed 3PL and generalized partial credit model
psych::describe(resp)
maxK <- apply( resp, 2, max, na.rm=TRUE )
I <- ncol(resp)
# specify guessing parameters, including a prior distribution
est.guess <- 1:I
est.guess[ maxK > 1 ] <- 0
guess <- .2*(est.guess >0)
guess.prior <- matrix( 0, nrow=I, ncol=2 )
guess.prior[ est.guess  > 0, 1] <- 5
guess.prior[ est.guess  > 0, 2] <- 17

# fit model
mod3 <- TAM::tam.mml.3pl( resp, gammaslope.des="2PL", est.guess=est.guess, guess=guess,
           guess.prior=guess.prior,
           control=list( maxiter=100, Msteps=10, fac.oldxsi=0.1,
                        nodes=seq(-8,8,len=41) ),  est.variance=FALSE )
summary(mod3)

# information curves
imod3 <- TAM::IRT.informationCurves( mod3 )
imod3

#*** estimate model in mirt package
library(mirt)
itemtype <- rep("gpcm", I)
itemtype[ maxK==1] <- "3PL"
mod3b <- mirt::mirt(resp, 1, itemtype=itemtype, verbose=TRUE )
print(mod3b)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Information curves}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
