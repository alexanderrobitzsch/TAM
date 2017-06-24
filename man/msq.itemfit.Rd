\name{msq.itemfit}

\alias{msq.itemfit}
\alias{summary.msq.itemfit}
\alias{msq.itemfitWLE}
\alias{summary.msq.itemfitWLE}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean Squared Residual Based Item Fit Statistics (Infit, Outfit)
}


\description{
The function \code{msq.itemfit} computes computed the outfit and infit statistic 
for items or item groups. Contrary to \code{\link{tam.fit}}, the function 
\code{msq.itemfit} is not based on simulation from individual posterior distributions
but rather on evaluating the individual posterior. 

The function \code{msq.itemfit} also computes the outfit and infit statistics
but these are based on weighted likelihood estimates obtained from
\code{\link{tam.wle}}.
}


\usage{
msq.itemfit( object , fitindices=NULL, version=2 )

\method{summary}{msq.itemfit}(object, ... )

msq.itemfitWLE( tamobj , fitindices=NULL , ... )

\method{summary}{msq.itemfitWLE}(object, ... )
}



\arguments{
  \item{object}{
Object for which the classes \code{IRT.data}, 
  \code{IRT.posterior} and \code{predict} are defined.}
\item{fitindices}{Vector with parameter labels defining the item groups for
which the fit should be evaluated.}
\item{version}{The \pkg{Rcpp} function used for calculation. \code{version=1}
provides results compatible with \pkg{TAM} version \eqn{\le}1.99-6.
A pure \R routine can be requested by \code{version=3}.}
\item{tamobj}{Object of class \code{tam.mml}, \code{tam.mml.2pl}
  or \code{tam.mml.mfr}.
}
\item{\dots}{Further arguments to be passed}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List with following entries	
\item{itemfit}{Data frame with outfit and infit statistics.}
\item{summary_itemfit}{Summary statistics of outfit
and infit}
}


%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%% Alexander Robitzsch
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{tam.fit}} for simulation based assessment of item fit.

See also \code{eRm::itemfit} or \code{\link[mirt:itemfit]{mirt::itemfit}}.
}


\examples{	
#############################################################################
# EXAMPLE 1: Simulated data Rasch model
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
# compute WLEs
wmod1 <- TAM::tam.wle(mod1)$theta

#--- item fit from "msq.itemfit" function
fit1 <- TAM::msq.itemfit(mod1)
summary( fit1 )

\dontrun{
#--- item fit using simulation in "tam.fit"
fit0 <- TAM::tam.fit( mod1 )
summary(fit0)

#--- item fit based on WLEs
fit2a <- TAM::msq.itemfitWLE( mod1 )
summary(fit2a)

#++ fit assessment in mirt package
library(mirt)
mod1b <- mirt::mirt( dat , model=1 , itemtype="Rasch" , verbose=TRUE )
print(mod1b)
sirt::mirt.wrapper.coef(mod1b)
fmod1b <- mirt::itemfit(mod1b , Theta= as.matrix(wmod1,ncol=1) ,
                 Zh = TRUE, X2 = FALSE, S_X2 = FALSE )
cbind( fit2a$fit_data , fmod1b )

#++ fit assessment in eRm package
library(eRm)
mod1c <- eRm::RM( dat )
summary(mod1c)
eRm::plotPImap(mod1c)    # person-item map
pmod1c <- eRm::person.parameter(mod1c)
fmod1c <- eRm::itemfit(pmod1c)
print(fmod1c)
plot(fmod1c)

#--- define some item groups for fit assessment

# bases on evaluating the posterior
fitindices <- rep( paste0("IG",c(1,2)) , each=10)
fit2 <- TAM::msq.itemfit( mod1 , fitindices )
summary(fit2)

# using WLEs
fit2b <- TAM::msq.itemfitWLE( mod1 , fitindices )
summary(fit2b)
}	

#############################################################################
# EXAMPLE 2: data.read | fit statistics assessed for testlets
############################################################################# 

library(sirt)
data(data.read,package="sirt")
dat <- data.read

# fit Rasch model
mod <- TAM::tam.mml( dat )

#***** item fit for each item
# based on posterior
res1 <- TAM::msq.itemfit( mod  )
summary(res1)
# based on WLEs
res2 <- TAM::msq.itemfitWLE( mod  )
summary(res2)

#***** item fit for item groups
# define item groups
fitindices <- substring( colnames(dat) , 1 , 1 )
# based on posterior
res1 <- TAM::msq.itemfit( mod , fitindices )
summary(res1)
# based on WLEs
res2 <- TAM::msq.itemfitWLE( mod , fitindices )
summary(res2)

\dontrun{
#############################################################################
# EXAMPLE 3: Fit statistics for rater models
############################################################################# 

library(sirt)
data(data.ratings2, package="sirt")
dat <- data.ratings2

# fit rater model "~ item*step + rater"
mod <- TAM::tam.mml.mfr( resp= dat[ , paste0( "k",1:5) ] , 
            facets= dat[ , "rater" , drop=FALSE] ,
            pid= dat$pid , formulaA = ~ item*step + rater )

# fit for parameter with "tam.fit" function
fmod1a <- TAM::tam.fit( mod )
fmod1b <- TAM::msq.itemfit( mod )
summary(fmod1a)
summary(fmod1b)

# define item groups using pseudo items from object "mod"
pseudo_items <- colnames(mod$resp)
pss <- strsplit( pseudo_items , split="-" )
item_parm <- unlist( lapply( pss , FUN = function(ll){ ll[1] } ) )
rater_parm <- unlist( lapply( pss , FUN = function(ll){ ll[2] } ) )

# fit for items with "msq.itemfit" functions
res2a <- TAM::msq.itemfit( mod , item_parm )
res2b <- TAM::msq.itemfitWLE( mod , item_parm )
summary(res2a)
summary(res2b)

# fit for raters
res3a <- TAM::msq.itemfit( mod , rater_parm )
res3b <- TAM::msq.itemfitWLE( mod , rater_parm )
summary(res3a)
summary(res3b)
}	
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Item fit}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
