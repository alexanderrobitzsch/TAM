%% File Name: tam.threshold.Rd
%% File Version: 2.16
%% File Last Change: 2017-06-22 17:41:52

\name{tam.threshold}
\alias{tam.threshold}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of Thurstonian Thresholds
}
\description{
This function estimates Thurstonian thresholds for item category
parameters of (generalized) partial credit models (see Details).
}
\usage{
tam.threshold(tamobj, prob.lvl=0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tamobj}{
Object of class \code{tam}
}
\item{prob.lvl}{
A numeric specifying the probability level of the threshold. 
The default is \code{prob.lvl=0.5}.  
}
}

\details{
This function only works appropriately for unidimensional models
or between item multidimensional models.
}

\value{
A data frame with Thurstonian thresholds. Rows correspond to items and
columns to item steps.
}

%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%% \pkg{TAM} authors
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the \pkg{WrightMap} package and Example 3 for creating Wright maps
with fitted models in \pkg{TAM}, see 
\code{\link[WrightMap:wrightMap]{wrightMap}}. 	
}

\examples{
#############################################################################
# EXAMPLE 1: ordered data - Partial credit model
#############################################################################
data( data.gpcm )

# Model 1: partial credit model
mod1 <- TAM::tam.mml( resp=data.gpcm ,control=list( maxiter=200) )
summary(mod1)
  ##   Item Parameters -A*Xsi
  ##        item   N     M AXsi_.Cat1 AXsi_.Cat2 AXsi_.Cat3 B.Cat1.Dim1 B.Cat2.Dim1 B.Cat3.Dim1
  ##   1 Comfort 392 0.880     -1.302      1.154      3.881           1           2           3
  ##   2    Work 392 1.278     -1.706     -0.847      0.833           1           2           3
  ##   3 Benefit 392 1.163     -1.233     -0.404      1.806           1           2           3

# Calculation of Thurstonian thresholds
TAM::tam.threshold(mod1)
  ##                Cat1      Cat2     Cat3
  ##   Comfort -1.325226 2.0717468 3.139801
  ##   Work    -1.777679 0.6459045 1.971222
  ##   Benefit -1.343536 0.7491760 2.403168
   
\dontrun{
#############################################################################
# EXAMPLE 2: Multidimensional model data.math
#############################################################################
	
library(sirt)
data(data.math, package="sirt")
dat <- data.math$data
# select items
items1 <- grep("M[A-D]" , colnames(dat) , value=TRUE)
items2 <- grep("M[H-I]" , colnames(dat) , value=TRUE)
# select dataset
dat <- dat[ c(items1,items2)]
# create Q-matrix
Q <- matrix( 0 , nrow=ncol(dat) , ncol=2 )
Q[ seq(1,length(items1) ) , 1 ] <- 1
Q[ length(items1) + seq(1,length(items2) ) , 2 ] <- 1

# fit two-dimensional model
mod1 <- TAM::tam.mml( dat , Q=Q )
# compute thresholds (specify a probability level of .625)
tmod1 <- TAM::tam.threshold( mod1  , prob.lvl = .625 )

#############################################################################
# EXAMPLE 3: Creating Wright maps with the WrightMap package
#############################################################################

library(WrightMap)
# For conducting Wright maps in combination with TAM, see
# http://wrightmap.org/post/100850738072/using-wrightmap-with-the-tam-package
data(sim.rasch)
dat <- sim.rasch

# estimate Rasch model in TAM
mod1 <- TAM::tam.mml(dat)
summary(mod1)

#--- A: creating a Wright map with WLEs

# compute WLE
wlemod1 <- TAM::tam.wle(mod1)$theta
# extract thresholds
tmod1 <- TAM::tam.threshold(mod1)
# create Wright map
WrightMap::wrightMap( thetas = wlemod1 , thresholds= tmod1 , label.items.srt=-90)

#--- B: creating a Wright Map with population distribution

# extract ability distribution and replicate observations
uni.proficiency <- rep( mod1$theta[,1] , round( mod1$pi.k * mod1$ic$n) )
# draw WrightMap
WrightMap::wrightMap( thetas=uni.proficiency, thresholds= tmod1 , label.items.rows=3)
}	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Thurstonian thresholds}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
