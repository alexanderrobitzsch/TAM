%% File Name: tam.fit.Rd
%% File Version: 2.46

\name{tam.fit}

\alias{tam.fit}
\alias{tam.mml.fit}
\alias{tam.jml.fit}
\alias{summary.tam.fit}

\title{
Item Infit and Outfit Statistic
}

\description{
The item infit and outfit statistic are calculated for
objects of classes \code{tam}, \code{tam.mml} and
\code{tam.jml},  respectively.
}

\usage{
tam.fit(tamobj, ...)

tam.mml.fit(tamobj, FitMatrix=NULL, Nsimul=NULL,progress=TRUE,
   useRcpp=TRUE, seed=NA, fit.facets=TRUE)

tam.jml.fit(tamobj)

\method{summary}{tam.fit}(object, \dots)
}

%% tam.jml.fit(tamobj, resp, resp.ind, A, B, nstud, nitems, maxK, ItemScore, 
%%    theta, xsi, Msteps, pweightsM, est.xsi.index)


\arguments{
  \item{tamobj}{
  An object of class \code{tam}, \code{tam.mml} or \code{tam.jml}
}
 \item{FitMatrix}{
 A fit matrix \eqn{F} for a specific hypothesis of fit of the linear function
\eqn{F \xi } (see Simulated Example 3 and Adams & Wu 2007).
  }
\item{Nsimul}{Number of simulations used for fit calculation.
The default is 100 (less than 400 students), 40 (less than 1000 students),
15 (less than 3000 students) and 5 (more than 3000 students)}
 \item{progress}{
    An optional logical indicating whether computation progress should
    be displayed at console.
}
\item{useRcpp}{Optional logical indicating whether \pkg{Rcpp}
	or pure \R code should be used for fit calculation.
The latter is consistent with \pkg{TAM} (<=1.1).}
\item{seed}{Fixed simulation seed.}
\item{fit.facets}{An optional logical indicating whether
fit for all facet parameters should be computed.}
\item{object}{Object of class \code{tam.fit}}
\item{\dots}{Further arguments to be passed}
}

%% \details{
%% Item fit is automatically calculated in JML estimation using
%% \code{\link{tam.jml}}.
%% }

\value{
In case of \code{tam.mml.fit} a data frame as entry \code{itemfit} 
with four columns:
\item{Outfit}{Item outfit statistic}
\item{Outfit_t}{The \eqn{t} value for the outfit statistic}
\item{Outfit_p}{Significance \eqn{p} value for outfit statistic}
\item{Outfit_pholm}{Significance \eqn{p} value for outfit statistic, 
adjusted for multiple testing according to the Holm procedure}
\item{Infit}{Item infit statistic}
\item{Infit_t}{The \eqn{t} value for the infit statistic}
\item{Infit_p}{Significance \eqn{p} value for infit statistic}
\item{Infit_pholm}{Significance \eqn{p} value for infit statistic, 
adjusted for multiple testing according to the Holm procedure}
}

\references{
Adams, R. J., & Wu, M. L. (2007). The mixed-coefficients multinomial logit model.
A generalized form of the Rasch model. In M. von Davier & C. H. Carstensen (Eds.).
\emph{Multivariate and mixture distribution Rasch models: Extensions and applications}
(pp. 55-76). New York: Springer.
}

%\author{
%%  ~~who you are~~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Fit statistics can be also calculated by the function \code{\link{msq.itemfit}}
which avoids simulations and directly evaluates individual
posterior distributions.

See \code{\link{tam.jml.fit}} for calculating item fit and person fit statistics
for models fitted with JML.	
    
See \code{\link{tam.personfit}} for computing person fit statistics.    
    
Item fit and person fit based on estimated person parameters can also be 
calculated using the \code{\link[sirt:pcm.fit]{sirt::pcm.fit}} function
in the \pkg{sirt} package (see Example 1 and Example 2).
}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
#############################################################################
# EXAMPLE 1: Dichotomous data data.sim.rasch
#############################################################################

data(data.sim.rasch)
# estimate Rasch model
mod1 <- TAM::tam.mml(resp=data.sim.rasch) 
# item fit
fit1 <- TAM::tam.fit( mod1 )
summary(fit1)
  ##   > summary(fit1)
  ##      parameter Outfit Outfit_t Outfit_p Infit Infit_t Infit_p
  ##   1         I1  0.966   -0.409    0.171 0.996  -0.087   0.233
  ##   2         I2  1.044    0.599    0.137 1.029   0.798   0.106
  ##   3         I3  1.022    0.330    0.185 1.012   0.366   0.179
  ##   4         I4  1.047    0.720    0.118 1.054   1.650   0.025

#--------
# infit and oufit based on estimated WLEs
library(sirt)

# estimate WLE
wle <- TAM::tam.wle(mod1)
# extract item parameters
b1 <- - mod1$AXsi[ , -1 ]
# assess item fit and person fit
fit1a <- sirt::pcm.fit(b=b1, theta=wle$theta , data.sim.rasch )
fit1a$item       # item fit statistic
fit1a$person     # person fit statistic

\dontrun{
#############################################################################
# EXAMPLE 2: Partial credit model data.gpcm
#############################################################################

data( data.gpcm )
dat <- data.gpcm

# estimate partial credit model in ConQuest parametrization 'item+item*step'
mod2 <- TAM::tam.mml( resp=dat , irtmodel="PCM2" )
summary(mod2)
# estimate item fit
fit2 <- TAM::tam.fit(mod2)
summary(fit2)

#  => The first three rows of the data frame correspond to the fit statistics
#     of first three items Comfort, Work and Benefit.

#--------
# infit and oufit based on estimated WLEs
# compute WLEs
wle <- TAM::tam.wle(mod2)
# extract item parameters
b1 <- - mod2$AXsi[ , -1 ]
# assess fit
fit1a <- sirt::pcm.fit(b=b1 , theta=wle$theta , dat)
fit1a$item

#############################################################################
# EXAMPLE 3: Fit statistic testing for local independence
#############################################################################

# generate data with local dependence and User-defined fit statistics
set.seed(4888)
I <- 40		# 40 items
N <- 1000       # 1000 persons

delta <- seq(-2,2, len=I)
theta <- stats::rnorm(N, 0, 1)
# simulate data
prob <- stats::plogis(outer(theta, delta, "-"))
rand <- matrix( stats::runif(N*I), nrow=N, ncol=I)
resp <- 1*(rand < prob)
colnames(resp) <- paste("I" , 1:I, sep="")
  
#induce some local dependence
for (item in c(10, 20, 30)){ 
 #  20% of the item responses for items 11, 21 and 31
 #are made equal to the previous item 
  row <- round( stats::runif(0.2*N)*N + 0.5)     
  resp[row, item+1] <- resp[row, item]
}
  
#run TAM
mod1 <- TAM::tam.mml(resp)
  
#User-defined fit design matrix
F <- array(0, dim=c(dim(mod1$A)[1], dim(mod1$A)[2], 6))
F[,,1] <- mod1$A[,,10] + mod1$A[,,11]
F[,,2] <- mod1$A[,,12] + mod1$A[,,13]
F[,,3] <- mod1$A[,,20] + mod1$A[,,21]
F[,,4] <- mod1$A[,,22] + mod1$A[,,23]
F[,,5] <- mod1$A[,,30] + mod1$A[,,31]
F[,,6] <- mod1$A[,,32] + mod1$A[,,33]
fit <- TAM::tam.fit(mod1, FitMatrix=F)
summary(fit)

#############################################################################
# EXAMPLE 4: Fit statistic testing for items with differing slopes
#############################################################################

#*** simulate data
library(sirt)
set.seed(9875)
N <- 2000
I <- 20
b <- sample( seq( -2 , 2 , length=I ) )
a <- rep( 1, I )
# create some misfitting items
a[c(1,3)] <- c(.5 , 1.5 )
# simulate data
dat <- sirt::sim.raschtype( rnorm(N) , b=b , fixed.a=a )
#*** estimate Rasch model
mod1 <- TAM::tam.mml(resp=dat) 
#*** assess item fit by infit and outfit statistic
fit1 <- TAM::tam.fit( mod1 )$itemfit
round( cbind( "b"=mod1$item$AXsi_.Cat1 , fit1$itemfit[,-1] )[1:7,], 3 )  

#*** compute item fit statistic in mirt package
library(mirt)
library(sirt)
mod1c <- mirt::mirt( dat , model=1 , itemtype="Rasch" , verbose=TRUE)
print(mod1c)                      # model summary
sirt::mirt.wrapper.coef(mod1c)    # estimated parameters
fit1c <- mirt::itemfit(mod1c , method="EAP")    # model fit in mirt package
# compare results of TAM and mirt
dfr <- cbind( "TAM"=fit1 , "mirt"=fit1c[,-c(1:2)] )

# S-X2 item fit statistic (see also the output from mirt) 
library(CDM)
sx2mod1 <- CDM::itemfit.sx2( mod1 )
summary(sx2mod1)

# compare results of CDM and mirt
sx2comp <-  cbind( sx2mod1$itemfit.stat[ , c("S-X2" , "p") ]  , 
                    dfr[  , c("mirt.S_X2" , "mirt.p.S_X2") ] )
round(sx2comp , 3 )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Item fit}
\keyword{Infit}
\keyword{Outfit}
\keyword{Fit statistics}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
