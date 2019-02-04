%% File Name: tam.np.Rd
%% File Version: 0.316


\name{tam.np}
\alias{tam.np}
\alias{summary.tam.np}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Unidimensional Non- and Semiparametric Item Response Model
}


\description{
Conducts non- and semiparametric estimation of a unidimensional item response model
for a single group allowing polytomous item responses (Rossi, Wang & Ramsay, 2002).

For dichotomous data, the function also allows group lasso penalty
(\code{penalty_type="lasso"}; Breheny & Huang, 2015; Yang & Zhou, 2015) and a ridge penalty
(\code{penalty_type="ridge"}; Rossi et al., 2002)
which is applied to the nonlinear part of the basis expansion. This approach
automatically detects deviations from a 2PL or a 1PL model (see Examples 2 and 3).
See Details for model specification.
}


\usage{
tam.np( dat, probs_init=NULL, pweights=NULL, lambda=NULL, control=list(),
    model="2PL", n_basis=0, basis_type="hermite", penalty_type="lasso",
    pars_init=NULL, orthonormalize=TRUE)

\method{summary}{tam.np}(object, file=NULL, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Matrix of integer item responses (starting from zero)
}
\item{probs_init}{Array containing initial probabilities}
\item{pweights}{
Optional vector of person weights
}
\item{lambda}{Numeric or vector of regularization parameter}
\item{control}{
List of control arguments, see \code{\link{tam.mml}}.
}
\item{model}{Specified target model. Can be \code{"2PL"} or \code{"1PL"}.}
\item{n_basis}{Number of basis functions}
\item{basis_type}{Type of basis function: \code{"bspline"} for B-splines
or \code{"hermite"} for Gauss-Hermite polynomials
}
\item{penalty_type}{Lasso type penalty (\code{"lasso"}) or ridge
penalty (\code{"ridge"})
}
\item{pars_init}{Optional matrix of initial item parameters}
\item{orthonormalize}{Logical indicating whether basis functions should
be orthonormalized}
\item{object}{Object of class \code{tam.np}}
\item{file}{Optional file name for summary output}
\item{\dots}{Further arguments to be passed}
}

\details{
The basis expansion approach is applied for the logit transformation of item
response functions for dichotomous data. In more detail, it this assumed that
\deqn{P(X_i=1|\theta)=\psi( H_0(\theta) + H_1(\theta)}
where \eqn{H_0} is the target function type and \eqn{H_1} is the semiparametric
part which parameterizes model deviations. For the 2PL model (\code{model="2PL"})
it is \eqn{H_0(\theta)=d_i + a_i \theta } and for the 1PL model
(\code{model="1PL"}) we set \eqn{H_1(\theta)=d_i + 1 \cdot \theta }.
The model discrepancy is specified as a basis expansion approach
\deqn{H_1 ( \theta )=\sum_{h=1}^p \beta_{ih} f_h( \theta)} where \eqn{f_h} are
basis functions (possibly orthonormalized) and \eqn{\beta_{ih}} are
item parameters which should be estimated. Penalty functions are posed on the
\eqn{\beta_{ih}} coefficients. For the group lasso penalty, we specify the
penalty \eqn{J_{i,L1}=N \lambda \sqrt{p} \sqrt{ \sum_{h=1}^p \beta_{ih}^2 }} while for
the ridge penalty it is \eqn{J_{i,L2}=N \lambda \sum_{h=1}^p \beta_{ih}^2 }
(\eqn{N} denoting the sample size).
}

\value{
List containing several entries
\item{rprobs}{Item response probabilities}
\item{theta}{Used nodes for approximation of \eqn{\theta} distribution}
\item{n.ik}{Expected counts}
\item{like}{Individual likelihood}
\item{hwt}{Individual posterior}
\item{item}{Summary item parameter table}
\item{pars}{Estimated parameters}
\item{\dots}{Further values}
}

\references{
Breheny, P., & Huang, J. (2015). Group descent algorithms for nonconvex penalized linear
and logistic regression models with grouped predictors.
\emph{Statistics and Computing, 25}(2), 173-187.
http://dx.doi.org/10.1007/s11222-013-9424-2

Rossi, N., Wang, X., & Ramsay, J. O. (2002). Nonparametric item response function
estimates with the EM algorithm.
\emph{Journal of Educational and Behavioral Statistics, 27}(3), 291-317. \cr
http://dx.doi.org/10.3102/10769986027003291

Yang, Y., & Zou, H. (2015). A fast unified algorithm for solving group-lasso penalized
learning problems. \emph{Statistics and Computing, 25}(6), 1129-1141.
http://dx.doi.org/10.1007/s11222-014-9498-5
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Nonparametric item response models can also be estimated with the
\code{mirt::itemGAM} function in the \pkg{mirt} package and the
\code{KernSmoothIRT::ksIRT} in the \pkg{KernSmoothIRT} package.

See \code{\link{tam.mml}} and \code{\link{tam.mml.2pl}} for parametric item response
models.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Nonparametric estimation polytomous data
#############################################################################

data(data.cqc02, package="TAM")
dat <- data.cqc02

#** nonparametric estimation
mod <- TAM::tam.np(dat)

#** extractor functions for objects of class 'tam.np'
lmod <- IRT.likelihood(mod)
pmod <- IRT.posterior(mod)
rmod <- IRT.irfprob(mod)
emod <- IRT.expectedCounts(mod)

#############################################################################
# EXAMPLE 2: Semiparametric estimation and detection of item misfit
#############################################################################

#- simulate data with two misfitting items
set.seed(998)
I <- 10
N <- 1000
a <- stats::rnorm(I, mean=1, sd=.3)
b <- stats::rnorm(I, mean=0, sd=1)
dat <- matrix(NA, nrow=N, ncol=I)
colnames(dat) <- paste0("I",1:I)
theta <- stats::rnorm(N)
for (ii in 1:I){
    dat[,ii] <- 1*(stats::runif(N) < stats::plogis( a[ii]*(theta-b[ii] ) ))
}

#* first misfitting item with lower and upper asymptote
ii <- 1
l <- .3
u <- 1
b[ii] <- 1.5
dat[,ii] <- 1*(stats::runif(N) < l + (u-l)*stats::plogis( a[ii]*(theta-b[ii] ) ))

#* second misfitting item with non-monotonic item response function
ii <- 3
dat[,ii] <- (stats::runif(N) < stats::plogis( theta-b[ii] + .6*theta^2))

#- 2PL model
mod0 <- TAM::tam.mml.2pl(dat)

#- lasso penalty with lambda of .05
mod1 <- TAM::tam.np(dat, n_basis=4, lambda=.05)

#- lambda value of .03 using starting value of previous model
mod2 <- TAM::tam.np(dat, n_basis=4, lambda=.03, pars_init=mod1$pars)

#- lambda=.015
mod3 <- TAM::tam.np(dat, n_basis=4, lambda=.015, pars_init=mod2$pars)

#- lambda=.007
mod4 <- TAM::tam.np(dat, n_basis=4, lambda=.007, pars_init=mod3$pars)

#- lambda=.001
mod5 <- TAM::tam.np(dat, n_basis=4, lambda=.001, pars_init=mod4$pars)

#- final estimation using solution of mod3
eps <- .0001
lambda_final <- eps+(1-eps)*mod3$regularized   # lambda parameter for final estimate
mod3b <- TAM::tam.np(dat, n_basis=4, lambda=lambda_final, pars_init=mod3$pars)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod3b)
summary(mod4)

# compare models with respect to information criteria
IRT.compareModels(mod0, mod1, mod2, mod3, mod3b, mod4, mod5)

#-- compute item fit statistics RISE
# regularized solution
TAM::IRT.RISE(mod_p=mod1, mod_np=mod3)
# regularized solution, final estimation
TAM::IRT.RISE(mod_p=mod1, mod_np=mod3b, use_probs=TRUE)
TAM::IRT.RISE(mod_p=mod1, mod_np=mod3b, use_probs=FALSE)
# use TAM::IRT.RISE() function for computing the RMSD statistic
TAM::IRT.RISE(mod_p=mod1, mod_np=mod1, use_probs=FALSE)

#############################################################################
# EXAMPLE 3: Mixed 1PL/2PL model
#############################################################################

#* simulate data with 2 2PL items and 8 1PL items
set.seed(9877)
N <- 2000
I <- 10
b <- seq(-1,1,len=I)
a <- rep(1,I)
a[c(3,8)] <- c(.5, 2)
theta <- stats::rnorm(N, sd=1)
dat <- sirt::sim.raschtype(theta, b=b, fixed.a=a)

#- 1PL model
mod1 <- TAM::tam.mml(dat)
#- 2PL model
mod2 <- TAM::tam.mml.2pl(dat)
#- 2PL model with penalty on slopes
mod3 <- TAM::tam.np(dat, lambda=.04, model="1PL", n_basis=0)
summary(mod3)
#- final mixed 1PL/2PL model
lambda <- 1*mod3$regularized
mod4 <- TAM::tam.np(dat, lambda=lambda, model="1PL", n_basis=0)
summary(mod4)

IRT.compareModels(mod1, mod2, mod3, mod4)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
