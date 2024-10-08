%% File Name: tam.wle.Rd
%% File Version: 2.509

\name{tam.wle}

\alias{tam.wle}
\alias{tam.mml.wle}
\alias{tam.mml.wle2}
\alias{tam_jml_wle}
\alias{print.tam.wle}
\alias{summary.tam.wle}


\title{
Weighted Likelihood Estimation and Maximum Likelihood Estimation of
Person Parameters
}

\description{
Compute the weighted likelihood estimator (Warm, 1989)
for objects of classes \code{tam}, \code{tam.mml} and \code{tam.jml},
respectively.
}

\usage{
tam.wle(tamobj, ...)

tam.mml.wle( tamobj, score.resp=NULL, WLE=TRUE, adj=.3, Msteps=20,
       convM=.0001, progress=TRUE,    output.prob=FALSE )

tam.mml.wle2(tamobj, score.resp=NULL, WLE=TRUE, adj=0.3, Msteps=20, convM=1e-04,
        progress=TRUE, output.prob=FALSE, pid=NULL, theta_init=NULL )

tam_jml_wle(tamobj, resp, resp.ind, A, B, nstud, nitems, maxK, convM,
    PersonScores, theta, xsi, Msteps, WLE=FALSE, theta.fixed=NULL, progress=FALSE,
    output.prob=TRUE, damp=0, version=2, theta_proc=NULL)

\method{summary}{tam.wle}(object, file=NULL, digits=3, \dots)

\method{print}{tam.wle}(x, digits=3, \dots)
}

\arguments{
  \item{tamobj}{
     An object generated by \code{tam.mml} or \code{tam.jml}. The object can also
     be a list containing (at least the) inputs \code{AXsi}, \code{B} and
     \code{resp} and therefore allows WLE estimation without fitting models
     in \pkg{TAM}.
}
\item{score.resp}{An optional data frame for which WLEs or MLEs
    should be calculated. In case of the default \code{NULL},
    \code{resp} from \code{tamobj} (i.e. \code{tamobj$resp}) is chosen.
    Note that items in \code{score.resp} must be the same (and in the
    same order) as in \code{tamobj$resp}.
}
  \item{WLE}{
     A logical indicating whether the weighted likelihood estimate
     (WLE, \code{WLE=TRUE}) or the maximum likelihood estimate (MLE, \code{WLE=FALSE})
     should be used.
}
  \item{adj}{
     Adjustment in MLE estimation for extreme scores (i.e. all or none
     items were correctly solved). This argument is not used if
     \code{WLE=TRUE}.
}
  \item{Msteps}{
     Maximum number of iterations
}
  \item{convM}{
     Convergence criterion
  }
\item{progress}{Logical indicating whether progress should be displayed.}
\item{output.prob}{Logical indicating whether evaluated probabilities should
be included in the list of outputs.}
\item{pid}{Optional vector of person identifiers}
\item{theta_init}{Initial theta values}
\item{resp}{Data frame with item responses (only for \code{tam.jml.WLE})
}
\item{resp.ind}{Data frame with response indicators (only for \code{tam.jml.WLE})
}
\item{A}{Design matrix \eqn{A} (applies only to \code{tam.jml.WLE})
}
\item{B}{Design matrix \eqn{B} (applies only to \code{tam.jml.WLE})
}
\item{nstud}{Number of persons (applies only to \code{tam.jml.WLE})
}
\item{nitems}{Number of items (applies only to \code{tam.jml.WLE})
}
\item{maxK}{Maximum item score (applies only to \code{tam.jml.WLE})
}
\item{PersonScores}{A vector containing the sufficient statistics for the
    person parameters (applies only to \code{tam.jml.WLE})
}
\item{theta}{Initial \eqn{\theta} estimate (applies only to \code{tam.jml.WLE})
}
\item{xsi}{Parameter vector \eqn{\xi} (applies only to \code{tam.jml.WLE})
}
\item{theta.fixed}{Matrix for fixed person parameters \eqn{\theta}. The first
    column includes the index whereas the second column includes
the fixed value.}
\item{damp}{Numeric value between 0 and 1 indicating amount of dampening
increments in \eqn{\theta} estimates during iterations}
\item{version}{Integer with possible values 2 or 3. In case of missing item responses,
\code{version=3} will typically be more efficient.}
\item{theta_proc}{Function for processing \code{theta} within iterations.}
\item{\dots}{Further arguments to be passed}
\item{object}{Object of class \code{tam.wle}}
\item{x}{Object of class \code{tam.wle}}
\item{file}{Optional file name in which the object summary should be written.}
\item{digits}{Number of digits for rounding}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
For \code{tam.wle.mml} and \code{tam.wle.mml2}, it is a data frame with following
columns:
\item{pid}{Person identifier}
\item{PersonScores}{Score of each person}
\item{PersonMax}{Maximum score of each person}
\item{theta}{Weighted likelihood estimate (WLE) or MLE}
\item{error}{Standard error of the WLE or MLE}
\item{WLE.rel}{WLE reliability (same value for all persons)} \cr

For \code{tam.jml.WLE}, it is a list with following entries:
\item{theta}{Weighted likelihood estimate (WLE) or MLE}
\item{errorWLE}{Standard error of the WLE or MLE}
\item{meanChangeWLE}{Mean change between updated and previous ability
    estimates from last iteration}
}

\references{
Penfield, R. D., & Bergeron, J. M. (2005). Applying a weighted maximum
likelihood latent trait estimator to the generalized partial credit model.
\emph{Applied Psychological Measurement, 29}, 218-233.

Warm, T. A. (1989). Weighted likelihood estimation of ability in item
response theory. \emph{Psychometrika, 54}, 427-450.
\doi{10.1007/BF02294627}
}

%% doi Penfield (2005): \doi{10.1177%2F0146621604270412}

%\author{
%%  ~~who you are~~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the \code{PP::PP_gpcm} function
in the \pkg{PP} package for more person
parameter estimators for the partial credit model (Penfield & Bergeron, 2005).

See the S3 method \code{\link{IRT.factor.scores.tam}}.
}

\examples{
#############################################################################
# EXAMPLE 1: 1PL model, data.sim.rasch
#############################################################################

data(data.sim.rasch)
# estimate Rasch model
mod1 <- TAM::tam.mml(resp=data.sim.rasch)
# WLE estimation
wle1 <- TAM::tam.wle( mod1 )
  ## WLE Reliability=0.894

print(wle1)
summary(wle1)

# scoring for a different dataset containing same items (first 10 persons in sim.rasch)
wle2 <- TAM::tam.wle( mod1, score.resp=data.sim.rasch[1:10,])

#--- WLE estimation without using a TAM object

#* create an input list
input <- list( resp=data.sim.rasch, AXsi=mod1$AXsi, B=mod1$B )
#* estimation
wle2b <- TAM::tam.mml.wle2( input )

\dontrun{
#############################################################################
# EXAMPLE 2: 3-dimensional Rasch model | data.read from sirt package
#############################################################################

data(data.read, package="sirt")
# define Q-matrix
Q <- matrix(0,12,3)
Q[ cbind( 1:12, rep(1:3,each=4) ) ] <- 1
# redefine data: create some missings for first three cases
resp <- data.read
resp[1:2, 5:12] <- NA
resp[3,1:4] <- NA
  ##   > head(resp)
  ##      A1 A2 A3 A4 B1 B2 B3 B4 C1 C2 C3 C4
  ##   2   1  1  1  1 NA NA NA NA NA NA NA NA
  ##   22  1  1  0  0 NA NA NA NA NA NA NA NA
  ##   23 NA NA NA NA  1  0  1  1  1  1  1  1
  ##   41  1  1  1  1  1  1  1  1  1  1  1  1
  ##   43  1  0  0  1  0  0  1  1  1  0  1  0
  ##   63  1  1  0  0  1  0  1  1  1  1  1  1

# estimate 3-dimensional Rasch model
mod <- TAM::tam.mml( resp=resp, Q=Q, control=list(snodes=1000,maxiter=50) )
summary(mod)

# WLE estimates
wmod <- TAM::tam.wle(mod, Msteps=3)
summary(wmod)
  ##   head(round(wmod,2))
  ##      pid N.items PersonScores.Dim01 PersonScores.Dim02 PersonScores.Dim03
  ##   2    1       4                3.7                0.3                0.3
  ##   22   2       4                2.0                0.3                0.3
  ##   23   3       8                0.3                3.0                3.7
  ##   41   4      12                3.7                3.7                3.7
  ##   43   5      12                2.0                2.0                2.0
  ##   63   6      12                2.0                3.0                3.7
  ##      PersonMax.Dim01 PersonMax.Dim02 PersonMax.Dim03 theta.Dim01 theta.Dim02
  ##   2              4.0             0.6             0.6        1.06          NA
  ##   22             4.0             0.6             0.6       -0.96          NA
  ##   23             0.6             4.0             4.0          NA       -0.07
  ##   41             4.0             4.0             4.0        1.06        0.82
  ##   43             4.0             4.0             4.0       -0.96       -1.11
  ##   63             4.0             4.0             4.0       -0.96       -0.07
  ##      theta.Dim03 error.Dim01 error.Dim02 error.Dim03 WLE.rel.Dim01
  ##   2           NA        1.50          NA          NA          -0.1
  ##   22          NA        1.11          NA          NA          -0.1
  ##   23        0.25          NA        1.17        1.92          -0.1
  ##   41        0.25        1.50        1.48        1.92          -0.1
  ##   43       -1.93        1.11        1.10        1.14          -0.1

# (1) Note that estimated WLE reliabilities are not trustworthy in this example.
# (2) If cases do not possess any observations on dimensions, then WLEs
#     and their corresponding standard errors are set to NA.

#############################################################################
# EXAMPLE 3: Partial credit model | Comparison WLEs with PP package
#############################################################################

library(PP)
data(data.gpcm)
dat <- data.gpcm
I <- ncol(dat)

#****************************************
#*** Model 1: Partial Credit Model

# estimation in TAM
mod1 <- TAM::tam.mml( dat )
summary(mod1)

#-- WLE estimation in TAM
tamw1 <- TAM::tam.wle( mod1 )

#-- WLE estimation with PP package
# convert AXsi parameters into thres parameters for PP
AXsi0 <- - mod1$AXsi[,-1]
b <- AXsi0
K <- ncol(AXsi0)
for (cc in 2:K){
    b[,cc] <- AXsi0[,cc] - AXsi0[,cc-1]
}
# WLE estimation in PP
ppw1 <- PP::PP_gpcm( respm=as.matrix(dat),  thres=t(b), slopes=rep(1,I) )

#-- compare results
dfr <- cbind( tamw1[, c("theta","error") ], ppw1$resPP)
head( round(dfr,3))
  ##      theta error resPP.estimate resPP.SE nsteps
  ##   1 -1.006 0.973         -1.006    0.973      8
  ##   2 -0.122 0.904         -0.122    0.904      8
  ##   3  0.640 0.836          0.640    0.836      8
  ##   4  0.640 0.836          0.640    0.836      8
  ##   5  0.640 0.836          0.640    0.836      8
  ##   6 -1.941 1.106         -1.941    1.106      8
plot( dfr$resPP.estimate, dfr$theta, pch=16, xlab="PP", ylab="TAM")
lines( c(-10,10), c(-10,10) )

#****************************************
#*** Model 2: Generalized partial Credit Model

# estimation in TAM
mod2 <- TAM::tam.mml.2pl( dat, irtmodel="GPCM" )
summary(mod2)

#-- WLE estimation in TAM
tamw2 <- TAM::tam.wle( mod2 )

#-- WLE estimation in PP
# convert AXsi parameters into thres and slopes parameters for PP
AXsi0 <- - mod2$AXsi[,-1]
slopes <- mod2$B[,2,1]
K <- ncol(AXsi0)
slopesM <- matrix( slopes, I, ncol=K )
AXsi0 <- AXsi0 / slopesM
b <- AXsi0
for (cc in 2:K){
    b[,cc] <- AXsi0[,cc] - AXsi0[,cc-1]
}
# estimation in PP
ppw2 <- PP::PP_gpcm( respm=as.matrix(dat),  thres=t(b), slopes=slopes )

#-- compare results
dfr <- cbind( tamw2[, c("theta","error") ], ppw2$resPP)
head( round(dfr,3))
  ##      theta error resPP.estimate resPP.SE nsteps
  ##   1 -0.476 0.971         -0.476    0.971     13
  ##   2 -0.090 0.973         -0.090    0.973     13
  ##   3  0.311 0.960          0.311    0.960     13
  ##   4  0.311 0.960          0.311    0.960     13
  ##   5  1.749 0.813          1.749    0.813     13
  ##   6 -1.513 1.032         -1.513    1.032     13
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{WLE}
%% \keyword{MLE}
%% \keyword{Person parameter estimation}
%% \keyword{print}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
