%% File Name: tam.modelfit.Rd
%% File Version: 2.37

\name{tam.modelfit}
\alias{tam.modelfit}
\alias{summary.tam.modelfit}

\alias{IRT.modelfit.tam.mml}
\alias{IRT.modelfit.tam.mml.3pl}
\alias{IRT.modelfit.tamaan}

\alias{summary.IRT.modelfit.tam.mml}
\alias{summary.IRT.modelfit.tam.mml.3pl}
\alias{summary.IRT.modelfit.tamaan}

\alias{tam.modelfit.IRT}
\alias{tam.modelfit.args}

\alias{tam.Q3}
\alias{summary.tam.Q3}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model Fit Statistics in \pkg{TAM}
}
\description{
The function \code{tam.modelfit} computes several model fit statistics.
It includes the Q3 statistic (Yen, 1984) and an
adjusted variant of it (see Details). Effect sizes of model fit
(\code{MADaQ3}, \eqn{MADRESIDCOV},
\eqn{SRMR}) are also available.

The function \code{IRT.modelfit} is a wrapper to \code{tam.modelfit},
but allows convenient model comparisons by using the
\code{\link[CDM:IRT.compareModels]{CDM::IRT.compareModels}} function.

The \code{tam.modelfit} function can also be used for fitted
models outside the \pkg{TAM} package by applying
\code{tam.modelfit.IRT} or \code{tam.modelfit.args}.

The function \code{tam.Q3} computes the \eqn{Q_3} statistic based on
weighted likelihood estimates (see \code{\link{tam.wle}}).
}


\usage{
tam.modelfit(tamobj, progress=TRUE)

\method{summary}{tam.modelfit}(object,...)

\method{IRT.modelfit}{tam.mml}(object, \dots)
\method{IRT.modelfit}{tam.mml.3pl}(object, \dots)
\method{IRT.modelfit}{tamaan}(object, \dots)

\method{summary}{IRT.modelfit.tam.mml}(object, \dots)
\method{summary}{IRT.modelfit.tam.mml.3pl}(object, \dots)
\method{summary}{IRT.modelfit.tamaan}(object, \dots)

tam.modelfit.IRT( object, progress=TRUE )

tam.modelfit.args( resp, probs, theta, post, progress=TRUE )

tam.Q3(tamobj, ... )

\method{summary}{tam.Q3}(object,...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tamobj}{
Object of class \code{tam}
}
  \item{progress}{
An optional logical indicating whether progress should
be displayed
}
\item{object}{Object of class \code{tam.modelfit} (for \code{summary})
  or objects for which \code{IRT.data}, \code{IRT.irfprob}
and \code{IRT.posterior} have been defined (for \code{tam.modelfit.IRT}).}
\item{resp}{Dataset with item responses}
\item{probs}{Array with item response functions evaluated at \code{theta}}
\item{theta}{Matrix with used \eqn{\bold{\theta}} grid}
\item{post}{Individual posterior distribution}
\item{\dots}{Further arguments to be passed}
}

\details{
For each item \eqn{i} and each person \eqn{n}, residuals
\eqn{e_{ni}=X_{ni}-E(X_{ni})} are computed. The expected
value \eqn{E(X_{ni})} is obtained by integrating the individual
posterior distribution.

The Q3 statistic of item pairs \eqn{i} and \eqn{j} is defined as the
correlation \eqn{Q3_{ij}=Cor( e_{ni}, e_{nj} )}. The residuals in
\code{tam.modelfit} are
calculated by integrating values of the individual posterior distribution.
Residuals in \code{tam.Q3} are calculated by using weighted likelihood
estimates (WLEs) from \code{\link{tam.wle}}.

It is known that under local independence, the expected value of \eqn{Q_3}
is slightly smaller than zero. Therefore,
an adjusted Q3 statistic (\code{aQ3}; \eqn{aQ3_{ij}})
is computed by subtracting the average of all Q3 statistics from
Q3. To control for multiple testing,
a p value adjustment by the method of
Holm (\code{p.holm}) is employed (see Chen, de la Torre & Zhang, 2013).

An effect size of model fit (\code{MADaQ3}) is defined as
the average of absolute values of \eqn{aQ3} statistics. An equivalent
statistic based on the \eqn{Q_3} statistic is similar to the
standardized generalized dimensionality discrepancy measure (SGDDM; Levy,
Xu, Yel & Svetina, 2015).

The SRMSR (standardized root mean square root of squared residuals,
Maydeu-Olivaras, 2013) is based on comparing residual correlations
of item pairs
\deqn{ SRMSR=\sqrt{ \frac{1}{ J(J-1)/2 } \sum_{i < j}
    ( r_{ij} - \hat{r}_{ij} )^2 } }
Additionally, the SRMR is computed as
\deqn{ SRMR=\frac{1}{ J(J-1)/2 } \sum_{i < j}
     | r_{ij} - \hat{r}_{ij} | }

The \eqn{MADRESIDCOV} statistic (McDonald & Mok, 1995) is based on comparing
residual covariances of item pairs
\deqn{ MADRESIDCOV=\frac{1}{ J(J-1)/2 } \sum_{i < j}
    | c_{ij} - \hat{c}_{ij} |  }
This statistic is just multiplied by 100 in the output of this function.
}


\value{
A list with following entries
  \item{stat.MADaQ3}{Global fit statistic \code{MADaQ3} and
  global model test with \code{p} value obtained
  by Holm adjustment}
  \item{chi2.stat}{Data frame with chi square tests of conditional independence
     for every item pair (Chen & Thissen, 1997)}
  \item{fitstat}{Model fit statistics \eqn{100 \cdot MADRESIDCOV},
      \eqn{SRMR} and \eqn{SRMSR}}
  \item{modelfit.test}{Test statistic of global fit based on multiple
  testing correction of \eqn{\chi^2} statistics
          }
  \item{stat.itempair}{Q3 and adjusted Q3 statistic for all item pairs}
  \item{residuals}{Residuals}
  \item{Q3.matr}{Matrix of \eqn{Q_3} statistics}
  \item{aQ3.matr}{Matrix of adjusted \eqn{Q_3} statistics}
\item{Q3_summary}{Summary of \eqn{Q_3} statistics}
\item{N_itempair}{Sample size for each item pair}
}

%  \item{chisquare.itemfit}{Item-wise \eqn{\chi ^2} local independence statistics}

%    res <- list( "stat.MADaQ3"=stat.MADaQ3, "chi2.stat"=chi2.stat,
%           "fitstat"=fitstat, "modelfit.test"=modelfit.test,
%           "stat.itempair"=dfr, "residuals"=residM )



\references{
Chen, J., de la Torre, J., & Zhang, Z. (2013).
Relative and absolute fit evaluation in cognitive diagnosis modeling.
\emph{Journal of Educational Measurement, 50}, 123-140.

Chen, W., & Thissen, D. (1997). Local dependence indexes for item pairs
using item response theory. \emph{Journal of Educational and Behavioral Statistics,
22}, 265-289.

Levy, R., Xu, Y., Yel, N., & Svetina, D. (2015). A standardized
generalized dimensionality discrepancy measure and a standardized model-based
covariance for dimensionality assessment for multidimensional models.
\emph{Journal of Educational Measurement, 52}(2), 144--158.

Maydeu-Olivares, A. (2013). Goodness-of-fit assessment of item response
theory models (with discussion).
\emph{Measurement: Interdisciplinary Research and Perspectives,
11}, 71-137.

McDonald, R. P., & Mok, M. M.-C. (1995). Goodness of fit in item response models.
\emph{Multivariate Behavioral Research, 30}, 23-40.

Yen, W. M. (1984). Effects of local item dependence on the fit and equating
performance of the three-parameter logistic model.
\emph{Applied Psychological Measurement, 8}, 125-145.
}

%\author{
%%  ~~who you are~~
%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
#############################################################################
# EXAMPLE 1: data.cqc01
#############################################################################

data(data.cqc01)
dat <- data.cqc01

#*****************************************************
#*** Model 1: Rasch model
mod1 <- TAM::tam.mml( dat )
# assess model fit
res1 <- TAM::tam.modelfit( tamobj=mod1 )
summary(res1)
# display item pairs with five largest adjusted Q3 statistics
res1$stat.itempair[1:5,c("item1","item2","aQ3","p","p.holm")]

\dontrun{
# IRT.modelfit
fmod1 <- IRT.modelfit(mod1)
summary(fmod1)

#*****************************************************
#*** Model 2: 2PL model
mod2 <- TAM::tam.mml.2pl( dat )
# IRT.modelfit
fmod2 <- IRT.modelfit(mod2)
summary(fmod2)

# model comparison
IRT.compareModels(fmod1, fmod2 )

#############################################################################
# SIMULATED EXAMPLE 2: Rasch model
#############################################################################

set.seed(8766)
N <- 1000    # number of persons
I <- 20      # number of items
# simulate responses
library(sirt)
dat <- sirt::sim.raschtype( rnorm(N), b=seq(-1.5,1.5,len=I) )
#*** estimation
mod1 <- TAM::tam.mml( dat )
summary(dat)
#*** model fit
res1 <- TAM::tam.modelfit( tamobj=mod1)
summary(res1)

#############################################################################
# EXAMPLE 3: Model fit data.gpcm | Partial credit model
#############################################################################

data(data.gpcm)
dat <- data.gpcm

# estimate partial credit model
mod1 <- TAM::tam.mml( dat)
summary(mod1)

# assess model fit
tmod1 <- TAM::tam.modelfit( mod1 )
summary(tmod1)

#############################################################################
# EXAMPLE 4: data.read | Comparison Q3 statistic
#############################################################################

library(sirt)
data(data.read, package="sirt")
dat <- data.read

#**** Model 1: 1PL model
mod1 <- TAM::tam.mml( dat )
summary(mod1)

#**** Model 2: 2PL model
mod2 <- TAM::tam.mml.2pl( dat )
summary(mod2)

#**** assess model fits
# Q3 based on posterior
fmod1 <- TAM::tam.modelfit(mod1)
fmod2 <- TAM::tam.modelfit(mod2)
# Q3 based on WLEs
q3_mod1 <- TAM::tam.Q3(mod1)
q3_mod2 <- TAM::tam.Q3(mod2)
summary(fmod1)
summary(fmod2)
summary(q3_mod1)
summary(q3_mod2)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Q3 statistic}
%% \keyword{Model fit}
%% \keyword{summary}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
