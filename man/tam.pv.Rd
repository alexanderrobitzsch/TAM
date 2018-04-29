%% File Name: tam.pv.Rd
%% File Version: 2.59

\name{tam.pv}
\alias{tam.pv}
\alias{tam.pv.mcmc}
\alias{summary.tam.pv.mcmc}
\alias{plot.tam.pv.mcmc}

\title{
Plausible Value Imputation
}

\description{
Plausible value imputation for objects of the classes \code{tam} and \code{tam.mml}
(Adams & Wu, 2007). For converting generated plausible values into
a list of multiply imputed datasets see \code{\link{tampv2datalist}}
and the Examples 2 and 3 of this function.

The function \code{tam.pv.mcmc} employs fully Bayesian estimation for drawing
plausible values and is recommended in cases when the latent regression model
is unreliably estimated (multidimensional model with stochastic nodes).
The parameters of the latent regression (regression coefficients and
residual covariance matrices) are drawn by Bayesian bootstrap (Rubin, 1981).
Either case probabilities (i.e., non-integer weights for cases in resampling;
argument \code{sample_integers=FALSE}) or ordinary
bootstrap (i.e., sampling cases with replacement; argument \code{sample_integers=TRUE})
can be used for the Bootstrap step by obtaining posterior draws of regression parameters.
}

\usage{
tam.pv(tamobj, nplausible = 10, ntheta = 2000, normal.approx = FALSE,
    samp.regr = FALSE, theta.model=FALSE, np.adj=8, na.grid = 5, verbose=TRUE)

tam.pv.mcmc( tamobj, Y=NULL, group=NULL, beta_groups = TRUE, nplausible=10, level = .95,
    n.iter = 1000, n.burnin = 500, adj_MH = .5, adj_change_MH = .05, refresh_MH = 50,
    accrate_bound_MH = c(.45, .55), sample_integers = FALSE, theta_init = NULL,
    print_iter = 20, verbose = TRUE, calc_ic = TRUE)

\method{summary}{tam.pv.mcmc}(object, file=NULL, \dots)

\method{plot}{tam.pv.mcmc}(x, \dots)
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ARGUMENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\arguments{
  \item{tamobj}{
     Object of class \code{tam} or \code{tam.mml}. For \code{tam.pv.mcmc}, it must not
     be an object of this class but rather a list with (at least the)
     entries \code{AXsi}, \code{B}, \code{resp}.
}
 \item{nplausible}{
     Number of plausible values to be drawn
}
\item{ntheta}{
    Number of ability nodes for plausible value imputation. Note
    that in this function ability nodes are simulated for the
    whole sample, not for every person (contrary to the software ConQuest).
}
  \item{normal.approx}{
     An optional logical indicating whether the individual posterior distributions
     should be approximated by a normal distribution?
     The default is \code{FALSE}. In the case \code{normal.approx=TRUE}
     (normal distribution approximation), the number of ability nodes
     \code{ntheta} can be substantially smaller than 2000, say 200 or 500.
     The normal approximation is implemented for unidimensional and
     multidimensional models.
}
  \item{samp.regr}{
     An optional logical indicating whether regression coefficients
     should be fixed in the plausible value imputation or
     also sampled from their posterior distribution?
     The default is \code{FALSE}. Sampled regression coefficients are
     obtained by nonparametric bootstrap.
}
\item{theta.model}{Logical indicating whether the theta grid from the
    \code{tamobj} object should be used for plausible value
    imputation. In case of \code{normal.approx=TRUE}, this should
    be sufficient in many applications.
}
  \item{np.adj}{
     This parameter defines the "spread" of the random theta values
     for drawing plausible values when \code{normal.approx=FALSE}.
     If \eqn{s_{EAP}} denotes the standard deviation of the posterior
     distribution of theta (in the one-dimensional case), then theta
     is simulated from a normal distribution with standard deviation
     \code{np.adj} times \eqn{s_{EAP}}.
        }
\item{na.grid}{Range of the grid in normal approximation. Default is from
-5 to 5.}
\item{\dots}{Further arguments to be passed}
\item{Y}{Optional matrix of regressors}
\item{group}{Optional vector of group identifiers}
\item{beta_groups}{Logical indicating whether group specific beta coefficients
   shall be estimated.}
\item{level}{Confidence level}
\item{n.iter}{Number of iterations}
\item{n.burnin}{Number of burnin-iterations}
\item{adj_MH}{Adjustment factor for Metropolis-Hastings (MH) steps which controls
    the variance of the proposal distribution for \eqn{\theta}. Can be also
    a vector of length equal to the number of persons.}
\item{adj_change_MH}{Allowed change for MH adjustment factor after refreshing}
\item{refresh_MH}{Number of iterations after which the variance of the proposal
 distribution should be updated}
\item{accrate_bound_MH}{Bounds for target acceptance rates of sampled \eqn{\theta}
   values.}
\item{sample_integers}{Logical indicating whether weights for complete cases
should be sampled in bootstrap}
\item{theta_init}{Optional matrix with initial \eqn{\bold{\theta}} values}
\item{print_iter}{Print iteration progress every \code{print_iter}th iteration}
\item{verbose}{Logical indicating whether iteration progress should be displayed.}
\item{calc_ic}{Logical indicating whether information criteria should be computed.}
\item{object}{Object of class \code{tam.pv.mcmc}}
\item{x}{Object of class \code{tam.pv.mcmc}}
\item{file}{A file name in which the summary output will be written}
}


%\details{
%% For details about the implemented method see Adams and Wu (2007).
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VALUES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\value{
The value of \code{tam.pv} is a list with following entries:
  \item{pv}{
    A data frame containing a person identifier (\code{pid})
    and plausible values denoted by \code{PVxx.Dimyy} which
    is the \code{xx}th plausible value of
    dimension \code{yy}.
        }
  \item{hwt}{Individual posterior distribution evaluated at
    the ability grid \code{theta}
    }
  \item{hwt1}{Cumulated individual posterior distribution}
  \item{theta}{Simulated ability nodes}

The value of \code{tam.pv.mcmc} is a list containing entries
\item{pv}{Data frame containing plausible values}
\item{parameter_samples}{Sampled regression parameters}
\item{ic}{Information criteria}
\item{beta}{Estimate of regression parameters \eqn{\bold{\beta}}}
\item{variance}{Estimate of residual variance matrix \eqn{\bold{\Sigma}}}
\item{correlation}{Estimate of residual correlation matrix corresponding to
\code{variance}}
\item{theta_acceptance_MH}{Acceptance rates and acceptance MH factors for each
individual}
\item{theta_last}{Last sampled \eqn{\bold{\theta}} value}
\item{\dots}{Further values}
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REFERENCES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\references{
Adams, R. J., & Wu, M. L. (2007). The mixed-coefficients multinomial logit model.
A generalized form of the Rasch model. In M. von Davier & C. H. Carstensen (Eds.):
\emph{Multivariate and mixture distribution Rasch models: Extensions and applications}
(pp. 55-76). New York: Springer.

Rubin, D. B. (1981). The Bayesian bootstrap. \emph{The Annals of Statistics, 9}(1),
130-134.
}


%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{tam.latreg}} for further examples of
fitting latent regression models and drawing plausible values
from models which provides an individual likelihood as an input.
}

\examples{
#############################################################################
# EXAMPLE 1: Dichotomous unidimensional data sim.rasch
#############################################################################

data(data.sim.rasch)
resp <- data.sim.rasch[ 1:500 , 1:15 ]  # select subsample of students and items

# estimate Rasch model
mod <- TAM::tam.mml(resp)

# draw 5 plausible values without a normality
# assumption of the posterior and 2000 ability nodes
pv1a <- TAM::tam.pv( mod , nplausible=5 , ntheta=2000 )

# draw 5 plausible values with a normality
# assumption of the posterior and 500 ability nodes
pv1b <- TAM::tam.pv( mod , nplausible=5 , ntheta=500 , normal.approx=TRUE )

# distribution of first plausible value from imputation pv1
hist(pv1a$pv$PV1.Dim1 )
# boxplot of all plausible values from imputation pv2
boxplot(pv1b$pv[ , 2:6 ] )

\dontrun{
# draw plausible values with tam.pv.mcmc function
Y <- matrix(1, nrow=500, ncol=1)
pv2 <- TAM::tam.pv.mcmc( tamobj=mod, Y=Y, nplausible=5 )
str(pv2)

# summary output
summary(pv2)
# assessing convergence with traceplots
plot(pv2, ask=TRUE)

# use starting values for theta and MH factors which fulfill acceptance rates
# from previously fitted model
pv3 <- TAM::tam.pv.mcmc( tamobj=mod, Y=Y, nplausible=5, theta_init = pv2$theta_last,
            adj_MH = pv2$theta_acceptance_MH$adj_MH )

#############################################################################
# EXAMPLE 2: Unidimensional plausible value imputation with
#            background variables; dataset data.pisaRead from sirt package
#############################################################################

data(data.pisaRead, package="sirt")
dat <- data.pisaRead$data
  ##   > colnames(dat)
  ##    [1] "idstud"   "idschool" "female"   "hisei"    "migra"    "R432Q01"
  ##    [7] "R432Q05"  "R432Q06"  "R456Q01"  "R456Q02"  "R456Q06"  "R460Q01"
  ##   [13] "R460Q05"  "R460Q06"  "R466Q02"  "R466Q03"  "R466Q06"

## Note that reading items have variable names starting with R4

# estimate 2PL model without covariates
items <- grep("R4" , colnames(dat) )    # select test items from data
mod2a <- TAM::tam.mml.2pl( resp=dat[,items] )
summary(mod2a)

# fix item parameters for plausible value imputation
   # fix item intercepts by defining xsi.fixed
xsi0 <- mod2a$xsi$xsi
xsi.fixed <- cbind( seq(1,length(xsi0)) , xsi0 )
   # fix item slopes using mod2$B
# matrix of latent regressors female, hisei and migra
Y <- dat[ , c("female" , "hisei" , "migra") ]
mod2b <- TAM::tam.mml( resp=dat[,items] , B=mod2a$B , xsi.fixed=xsi.fixed , Y=Y,
            pid = dat$idstud)

# plausible value imputation with normality assumption
# and ignoring uncertainty about regression coefficients
#    -> the default is samp.regr=FALSE
pv2c <- TAM::tam.pv( mod2b, nplausible=10, ntheta=500, normal.approx=TRUE )
# sampling of regression coefficients
pv2d <- TAM::tam.pv( mod2b, nplausible=10, ntheta=500, samp.regr=TRUE)
# sampling of regression coefficients, normal approximation using the
# theta grid from the model
pv2e <- TAM::tam.pv( mod2b, samp.regr=TRUE, theta.model=TRUE, normal.approx=TRUE)

#--- create list of multiply imputed datasets with plausible values
# define dataset with covariates to be matched
Y <- dat[ , c("idstud", "idschool", "female", "hisei", "migra") ]

# define plausible value names
pvnames <- c("PVREAD")
# create list of imputed datasets
datlist1 <- TAM::tampv2datalist( pv2e, pvnames = pvnames , Y=Y , Y.pid="idstud")
str(datlist1)

# create a matrix of covariates with different set of students than in pv2e
Y1 <- Y[ seq( 1 , 600 , 2 ) , ]
# create list of multiply imputed datasets
datlist2 <- TAM::tampv2datalist( pv2e, pvnames = c("PVREAD"), Y=Y1 , Y.pid="idstud")

#--- fit some models in lavaan and semTools
library(lavaan)
library(semTools)

#*** Model 1: Linear regression
lavmodel <- "
   PVREAD ~ migra + hisei
   PVREAD ~~ PVREAD
        "
mod1 <- semTools::lavaan.mi( lavmodel , data = datlist1 , m=0)
summary(mod1 , standardized=TRUE, rsquare=TRUE)

# apply lavaan for third imputed dataset
mod1a <- lavaan::lavaan( lavmodel , data = datlist1[[3]] )
summary(mod1a , standardized=TRUE, rsquare=TRUE)

# compare with mod1 by looping over all datasets
mod1b <- lapply( datlist1 , FUN = function(dat0){
    mod1a <- lavaan( lavmodel , data = dat0 )
    coef( mod1a)
        } )
mod1b
mod1b <- matrix( unlist( mod1b ) , ncol= length( coef(mod1)) , byrow=TRUE )
mod1b
round( colMeans(mod1b) , 3 )
coef(mod1)   # -> results coincide

#*** Model 2: Path model
lavmodel <- "
   PVREAD ~ migra + hisei
   hisei ~ migra
   PVREAD ~~ PVREAD
   hisei ~~ hisei
        "
mod2 <- semTools::lavaan.mi( lavmodel , data = datlist1 )
summary(mod2 , standardized=TRUE, rsquare=TRUE)
# fit statistics
inspect( mod2 , what="fit")

#--- using mitools package
library(mitools)
# convert datalist into an object of class imputationList
datlist1a <- mitools::imputationList( datlist1 )
# fit linear regression
mod1c <- with( datlist1a , stats::lm( PVREAD ~ migra + hisei ) )
summary( mitools::MIcombine(mod1c) )

#--- using mice package
library(mice)
library(miceadds)
# convert datalist into a mids object
mids1 <- miceadds::datalist2mids( datlist1 )
# fit linear regression
mod1c <- with( mids1 , stats::lm( PVREAD ~ migra + hisei ) )
summary( mice::pool(mod1c) )

#############################################################################
# EXAMPLE 3: Multidimensional plausible value imputation
#############################################################################

# (1) simulate some data
set.seed(6778)
library(mvtnorm)
N <- 1000
Y <- cbind( stats::rnorm(N) , stats::rnorm(N) )
theta <- mvtnorm::rmvnorm( N, mean=c(0,0), sigma=matrix( c(1,.5,.5,1) , 2 , 2 ))
theta[,1] <- theta[,1] + .4 * Y[,1] + .2 * Y[,2]  # latent regression model
theta[,2] <- theta[,2] + .8 * Y[,1] + .5 * Y[,2]  # latent regression model
I <- 20
p1 <- stats::plogis( outer( theta[,1] , seq( -2 , 2 , len=I ) , "-" ) )
resp1 <- 1 * ( p1 > matrix( stats::runif( N*I ) , nrow=N , ncol=I ) )
p1 <- stats::plogis( outer( theta[,2] , seq( -2 , 2 , len=I ) , "-" ) )
resp2 <- 1 * ( p1 > matrix( stats::runif( N*I ) , nrow=N , ncol=I ) )
resp <- cbind(resp1,resp2)
colnames(resp) <- paste("I" , 1:(2*I), sep="")

# (2) define loading Matrix
Q <- array( 0 , dim = c( 2*I , 2 ))
Q[cbind(1:(2*I), c( rep(1,I) , rep(2,I) ))] <- 1

# (3) fit latent regression model
mod <- TAM::tam.mml( resp=resp , Y=Y , Q=Q )

# (4) draw plausible values
pv1 <- TAM::tam.pv( mod , theta.model = TRUE )

# (5) convert plausible values to list of imputed datasets
Y1 <- data.frame(Y)
colnames(Y1) <- paste0("Y",1:2)
pvnames <- c("PVFA","PVFB")
# create list of imputed datasets
datlist1 <- TAM::tampv2datalist( pv1 , pvnames = pvnames , Y=Y1 )
str(datlist1)

# (6) apply statistical models
library(semTools)
# define linear regression
lavmodel <- "
   PVFA ~ Y1 + Y2
   PVFA ~~ PVFA
        "
mod1 <- semTools::lavaan.mi( lavmodel , data = datlist1 )
summary(mod1 , standardized=TRUE, rsquare=TRUE)

# (7) draw plausible values with tam.pv.mcmc function
Y1 <- cbind( 1, Y )
pv2 <- TAM::tam.pv.mcmc( tamobj = mod , Y = Y1, n.iter=1000, n.burnin=200 )

# (8) group-specific plausible values
set.seed(908)
# create artificial grouping variable
group <- sample( 1:3, N, replace=TRUE )
pv3 <- TAM::tam.pv.mcmc( tamobj , Y = Y1, n.iter=1000, n.burnin=200, group=group )

# (9) plausible values with no fitted object in TAM

# fit IRT model without covariates
mod4a <- TAM::tam.mml( resp=resp, Q=Q )
# define input for tam.pv.mcmc
tamobj1 <- list( AXsi=mod4a$AXsi , B=mod4a$B , resp=mod4a$resp )
pmod4 <- TAM::tam.pv.mcmc( tamobj1 , Y = Y1 )

#############################################################################
# EXAMPLE 4: Plausible value imputation with measurement errors in covariates
#############################################################################

library(sirt)
set.seed(7756)
N <- 2000    # number of persons
I <- 10     # number of items

# simulate covariates
X <- mvrnorm( N , mu=c(0,0) , Sigma = matrix( c(1,.5,.5,1) ,2 ,2 ) )
colnames(X) <- paste0("X",1:2)
# second covariate with measurement error with variance var.err
var.err <- .3
X.err <- X
X.err[,2] <-X[,2] + rnorm(N, sd = sqrt(var.err) )
# simulate theta
theta <- .5*X[,1] + .4*X[,2] + rnorm( N , sd = .5 )
# simulate item responses
itemdiff <- seq( -2 , 2 , length=I)  # item difficulties
dat <- sirt::sim.raschtype( theta , b = itemdiff )

#***********************
#*** Model 0: Regression model with true variables
mod0 <- stats::lm( theta ~ X )
summary(mod0)

#***********************
#*** Model 1: latent regression model with true covariates X
xsi.fixed <- cbind( 1:I , itemdiff )
mod1 <- TAM::tam.mml( dat , xsi.fixed=xsi.fixed , Y=X)
summary(mod1)

# draw plausible values
res1a <- TAM::tam.pv( mod1 , normal.approx=TRUE , ntheta=200 , samp.regr=TRUE)
# create list of multiply imputed datasets
library(miceadds)
datlist1a <- TAM::tampv2datalist( res1a , Y=X )
imp1a <- miceadds::datalist2mids( datlist1a )

# fit linear model
# linear regression with measurement errors in X
lavmodel <- "
   PV.Dim1 ~ X1 + X2true
   X2true =~ 1*X2
   X2 ~~ 0.3*X2  # = var.err
   PV.Dim1 ~~ PV.Dim1
   X2true ~~ X2true
        "
mod1a <- semTools::lavaan.mi( lavmodel , datlist1a)
summary(mod1a , standardized=TRUE, rsquare=TRUE)

#***********************
#*** Model 2: latent regression model with error-prone covariates X.err
mod2 <- TAM::tam.mml( dat , xsi.fixed=xsi.fixed , Y=X.err)
summary(mod2)

#***********************
#*** Model 3: Adjustment of covariates

cov.X.err <- cov( X.err )
# matrix of variance of measurement errors
measerr <- diag( c(0,var.err) )
# true covariance matrix
cov.X <- cov.X.err - measerr
# mean of X.err
mu <- colMeans(X.err)
muM <- matrix( mu , nrow=nrow(X.err) , ncol=ncol(X.err) , byrow=TRUE)
# reliability matrix
W <- solve( cov.X.err ) \%*\% cov.X
ident <- diag(2)
# adjusted scores of X
X.adj <- ( X.err - muM ) \%*\% W   + muM \%*\% ( ident - W )

# fit latent regression model
mod3 <- TAM::tam.mml( dat , xsi.fixed=xsi.fixed , Y=X.adj)
summary(mod3)

# draw plausible values
res3a <- TAM::tam.pv( mod3 , normal.approx=TRUE , ntheta=200 , samp.regr=TRUE)

# create list of multiply imputed datasets
library(semTools)

#*** PV dataset 1
# datalist with error-prone covariates
datlist3a <- TAM::tampv2datalist( res3a , Y=X.err )
# datalist with adjusted covariates
datlist3b <- TAM::tampv2datalist( res3a , Y=X.adj )

# linear regression with measurement errors in X
lavmodel <- "
   PV.Dim1 ~ X1 + X2true
   X2true =~ 1*X2
   X2 ~~ 0.3*X2  # = var.err
   PV.Dim1 ~~ PV.Dim1
   X2true ~~ X2true
        "
mod3a <- semTools::lavaan.mi( lavmodel , datlist3a)
summary(mod3a , standardized=TRUE, rsquare=TRUE)

lavmodel <- "
   PV.Dim1 ~ X1 + X2
   PV.Dim1 ~~ PV.Dim1
        "
mod3b <- semTools::lavaan.mi( lavmodel , datlist3b)
summary(mod3b , standardized=TRUE, rsquare=TRUE)
# => mod3b leads to the correct estimate.

#*********************************************
# plausible value imputation for abilities and error-prone
# covariates using the mice package

library(mice)
library(miceadds)

# creating the likelihood for plausible value for abilities
mod11 <- TAM::tam.mml( dat , xsi.fixed = xsi.fixed )
likePV <- IRT.likelihood(mod11)
# creating the likelihood for error-prone covariate X2
lavmodel <- "
  X2true =~ 1*X2
  X2 ~~ 0.3*X2
    "
mod12 <- lavaan::cfa( lavmodel , data = as.data.frame(X.err) )
summary(mod12)
likeX2 <- TAM::IRTLikelihood.cfa( data= X.err , cfaobj=mod12)
str(likeX2)

#-- create data input for mice package
data <- data.frame( "PVA" = NA , "X1" = X[,1] , "X2" = NA  )
vars <- colnames(data)
V <- length(vars)
predictorMatrix <- 1 - diag(V)
rownames(predictorMatrix) <- colnames(predictorMatrix) <- vars
imputationMethod <- rep("norm" , V )
names(imputationMethod) <- vars
imputationMethod[c("PVA","X2")] <- "2l.plausible.values"

#-- create argument lists for plausible value imputation
# likelihood and theta grid of plausible value derived from IRT model
like <- list( "PVA" = likePV  , "X2" = likeX2 )
theta <- list( "PVA" = attr(likePV,"theta") ,
                "X2" = attr(likeX2 , "theta") )
#-- initial imputations
data.init <- data
data.init$PVA <- mod11$person$EAP
data.init$X2 <- X.err[,"X2"]

#-- imputation using the mice and miceadds package
imp1 <- mice::mice( as.matrix(data) , predictorMatrix = predictorMatrix , m = 4, maxit = 6 ,
             imputationMethod = imputationMethod ,  allow.na = TRUE ,
             theta=theta , like=like , data.init=data.init )
summary(imp1)

# compute linear regression
mod4a <- with( imp1 , stats::lm( PVA ~ X1 + X2 ) )
summary( mice::pool(mod4a) )
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Plausible value imputation}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
