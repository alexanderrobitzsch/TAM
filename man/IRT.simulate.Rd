%% File Name: IRT.simulate.Rd
%% File Version: 0.14
%% File Last Change: 2017-06-22 17:41:51

\name{IRT.simulate}
\alias{IRT.simulate}
\alias{IRT.simulate.tam.mml}
\alias{IRT.simulate.tam.mml.2pl}
\alias{IRT.simulate.tam.mml.3pl}
\alias{IRT.simulate.tam.mml.mfr}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Simulating Item Response Models
}

\description{
Defines an S3 method for simulation of item response models.
}

\usage{
IRT.simulate(object, ...)

\method{IRT.simulate}{tam.mml}(object, iIndex=NULL, theta=NULL, nobs=NULL, \dots)

\method{IRT.simulate}{tam.mml.2pl}(object, iIndex=NULL, theta=NULL, nobs=NULL, \dots)

\method{IRT.simulate}{tam.mml.mfr}(object, iIndex=NULL, theta=NULL, nobs=NULL, \dots)

\method{IRT.simulate}{tam.mml.3pl}(object, iIndex=NULL, theta=NULL, nobs=NULL, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object of class \code{tam.mml}, \code{tam.mml.2pl}, \code{tam.mml.mfr} or
\code{tam.mml.3pl}.
}
\item{iIndex}{Optional vector of item indices}
\item{theta}{Optional matrix of \code{theta} values}
\item{nobs}{Optional numeric containing the number of observations to be simulated.}
  \item{\dots}{
Further objects to be passed
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Data frame with simulated item responses
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
# EXAMPLE 1: Simulating Rasch model
#############################################################################
	
data(data.sim.rasch)

#** (1) estimate model
mod1 <- TAM::tam.mml(resp = data.sim.rasch ) 

#** (2) simulate data
sim.dat <- TAM::IRT.simulate(mod1)

\dontrun{
#** (3) use a subset of items with the argument iIndex
set.seed(976)
iIndex <- sort(sample(ncol(data.sim.rasch), 15))  # randomly select 15 items
sim.dat <- TAM::IRT.simulate(mod1, iIndex = iIndex)
mod.sim.dat <- TAM::tam.mml(sim.dat)

#** (4) specify number of persons in addition
sim.dat <- TAM::IRT.simulate(mod1, nobs = 1500, iIndex = iIndex)

# Rasch - constraint = "items" ----
mod1 <- TAM::tam.mml(resp = data.sim.rasch,  constraint = "items" , 
                control=list( xsi.start0=1 , fac.oldxsi=.5) ) 

# provide abilities
theta0 <- matrix( rnorm(1500, mean=0.5, sd= sqrt(mod1$variance)) , ncol=1 )
# simulate data
data <- TAM::IRT.simulate(mod1, theta = theta0 )
# estimate model based on simulated data
xsi.fixed <- cbind(1:nrow(mod1$item), mod1$item$xsi.item)
mod2 <- TAM::tam.mml(data , xsi.fixed=xsi.fixed )
summary(mod2)

#############################################################################
# EXAMPLE 2: Simulating 2PL model
#############################################################################

data(data.sim.rasch)
# estimate 2PL
mod2 <- TAM::tam.mml.2pl(resp = data.sim.rasch, irtmodel = "2PL")
# simulate 2PL
sim.dat <- TAM::IRT.simulate(mod2)
mod.sim.dat <- TAM::tam.mml.2pl(resp = sim.dat, irtmodel = "2PL") 

#############################################################################
# EXAMPLE 3: Simulate multiple group model
#############################################################################

# Multi-Group ----
set.seed(6778)
N <- 3000
theta <- c( stats::rnorm(N/2,mean=0,sd = 1.5) , stats::rnorm(N/2,mean=.5,sd = 1)  )
I <- 20
p1 <- stats::plogis( outer( theta , seq( -2 , 2 , len=I ) , "-" ) )
resp <- 1 * ( p1 > matrix( stats::runif( N*I ) , nrow=N , ncol=I ) )
colnames(resp) <- paste("I" , 1:I, sep="")
group <- rep(1:2 , each=N/2 )
mod3 <- TAM::tam.mml(resp, group = group)

# simulate data
sim.dat.g1 <- TAM::IRT.simulate(mod3, 
                   theta = matrix( stats::rnorm(N/2, mean = 0, sd = 1.5), ncol=1) )
sim.dat.g2 <- TAM::IRT.simulate(mod3, 
                   theta = matrix( stats::rnorm(N/2, mean = .5, sd = 1), ncol=1) )
sim.dat <- rbind( sim.dat.g1 , sim.dat.g2)
# estimate model
mod3s <- TAM::tam.mml( sim.dat , group = group)

#############################################################################
# EXAMPLE 4: Multidimensional model and latent regression
#############################################################################

set.seed(6778)
N <- 2000      
Y <- cbind( stats::rnorm(N), stats::rnorm(N))
theta <- mvtnorm::rmvnorm(N, mean = c(0,0), sigma = matrix(c(1,.5,.5,1), 2, 2))
theta[,1] <- theta[,1] + .4 * Y[,1] + .2 * Y[,2]  # latent regression model
theta[,2] <- theta[,2] + .8 * Y[,1] + .5 * Y[,2]  # latent regression model
I <- 20
p1 <- stats::plogis(outer(theta[, 1], seq(-2, 2, len = I), "-"))
resp1 <- 1 * (p1 > matrix(stats::runif(N * I), nrow = N, ncol = I))
p1 <- stats::plogis(outer(theta[, 2], seq(-2 , 2 , len=I ) , "-" ))
resp2 <- 1 * (p1 > matrix(stats::runif(N * I), nrow = N, ncol = I))
resp <- cbind(resp1, resp2)
colnames(resp) <- paste("I", 1 : (2 * I), sep = "")

# (2) define loading Matrix
Q <- array(0 , dim = c(2 * I, 2))
Q[cbind(1:(2*I), c(rep(1, I), rep(2, I)))] <- 1
Q

# (3) estimate models 
mod4 <- TAM::tam.mml(resp = resp, Q = Q, Y = Y, control = list(  maxiter = 15))

# simulate new item responses 
theta <- mvtnorm::rmvnorm(N, mean = c(0,0), sigma = matrix(c(1,.5,.5,1), 2, 2))
theta[,1] <- theta[,1] + .4 * Y[,1] + .2 * Y[,2]  # latent regression model
theta[,2] <- theta[,2] + .8 * Y[,1] + .5 * Y[,2]  # latent regression model
  
sim.dat <- TAM::IRT.simulate(mod4, theta = theta)
mod.sim.dat <- TAM::tam.mml(resp = sim.dat, Q = Q, Y = Y, control = list( maxiter = 15))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
