%% File Name: tam.jml.Rd
%% File Version: 2.38

\name{tam.jml}

\alias{tam.jml}
\alias{summary.tam.jml}
\alias{logLik.tam.jml}

\title{
Joint Maximum Likelihood Estimation
}

\description{
This function estimate unidimensional
item response models with joint maximum likelihood (JML,
 see e.g. Linacre, 1994).
}

\usage{
tam.jml(resp, group=NULL, adj=.3, disattenuate=FALSE, bias=TRUE,
    xsi.fixed=NULL, xsi.inits=NULL, theta.fixed=NULL, A=NULL, B=NULL, Q=NULL,
    ndim=1, pweights=NULL, verbose=TRUE, control=list(), version=2)

\method{summary}{tam.jml}(object, file=NULL, \dots)

\method{logLik}{tam.jml}(object, \dots)
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ARGUMENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resp}{
     A matrix of item responses. Missing responses must be declared
     as \code{NA}.
}
  \item{group}{
     An optional vector of group identifier
}
  \item{disattenuate}{
    An optional logical indicating whether the person parameters
    should be disattenuated due to unreliability?
    The disattenuation is conducted by applying the Kelley formula.
}
\item{adj}{Adjustment constant which is subtracted or added to extreme
scores (score of zero or maximum score. The default is a value of 0.3}
  \item{bias}{
     A logical which indicates if JML bias should be reduced
     by multiplying item parameters by the adjustment factor
     of \eqn{(I-1)/I}
}
  \item{xsi.fixed}{
     An optional matrix with two columns for fixing some of the
     basis parameters \eqn{\xi} of item intercepts.
     1st column: Index of \eqn{\xi} parameter, 2nd column:
     Fixed value of \eqn{\xi} parameter
}
  \item{xsi.inits}{
     An optional vector of initial \eqn{\xi} parameters. Note that
     all parameters must be specified and the vector is not of the
     same format as \code{xsi.fixed}.
}
\item{theta.fixed}{Matrix for fixed person parameters \eqn{\theta}. The first
    column includes the index whereas the second column includes
the fixed value. This argument can only be applied for \code{version=1}.}
  \item{A}{
     A design array \eqn{A} for item category intercepts.
     For item \eqn{i} and category \eqn{k}, the threshold is
     specified as \eqn{ \sum _j a_{ikj}   \xi_j}.
}
  \item{B}{
     A design array for scoring item category responses.
     Entries in \eqn{B} represent item loadings on
     abilities \eqn{\theta}.
}
  \item{Q}{
     A Q-matrix which defines loadings of items on dimensions.
}

  \item{ndim}{
   Number of dimensions in the model. The default is 1.
}
  \item{pweights}{
   An optional vector of person weights.
}
\item{verbose}{Logical indicating whether output should
be printed during iterations. This argument replaces \code{control$progress}.
}
  \item{control}{
     A list of control arguments. See \code{\link{tam.mml}}
     for more details.
}
\item{version}{Version function which should be used. \code{version=2}
is the former \code{tam.jml2} function in \pkg{TAM} (<2.0).}
  \item{object}{
Object of class \code{tam.jml} (only for \code{summary.tam}
function)
}
\item{file}{
A file name in which the summary output will be written
(only for \code{summary.tam.jml} function)
    }
\item{\dots}{
    Further arguments to be passed
    }

}

%\details{
%The function \code{tam.jml2} is just a bit more efficient (for most
%applications) implementation than \code{tam.jml}.
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VALUES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\value{
A list with following entries
\item{item}{Data frame with item parameters}
\item{xsi}{Vector of item parameters \eqn{\xi}}
\item{errorP}{Standard error of item parameters \eqn{\xi}}
\item{theta}{MLE in final step}
\item{errorWLE}{Standard error of WLE}
\item{WLE}{WLE in last iteration}
\item{WLEreliability}{WLE reliability}
\item{PersonScores}{Scores for each person (sufficient statistic)}
\item{ItemScore}{Sufficient statistic for each item parameter}
\item{PersonMax}{Maximum person score}
\item{ItemMax}{Maximum item score}
\item{deviance}{Deviance}
\item{deviance.history}{Deviance history in iterations}
\item{resp}{Original data frame}
\item{resp.ind}{Response indicator matrix}
\item{group}{Vector of group identifiers (if provided as an argument)}
\item{pweights}{Vector of person weights}
\item{A}{Design matrix \eqn{A} of item intercepts}
\item{B}{Loading (or scoring) matrix \eqn{B}}
\item{nitems}{Number of items}
\item{maxK}{Maximum number of categories}
\item{nstud}{Number of persons in \code{resp}}
\item{resp.ind.list}{Like \code{resp.ind}, only in the format of a list}
\item{xsi.fixed}{Fixed \eqn{\xi} item parameters}
\item{control}{Control list}
\item{\dots}{}
}

%\item{outfitPerson}{Person outfit statistic}
%\item{outfitItem}{Item outfit statistic}
%\item{infitPerson}{Person infit statistic}
%\item{infitItem}{Item infit statistic}
%\item{outfitPerson_t}{The \eqn{t} value of person outfit}
%\item{outfitItem_t}{The \eqn{t} value of item outfit}
%\item{infitPerson_t}{The \eqn{t} value of person infit}
%\item{infitItem_t}{The \eqn{t} value of item infit}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REFERENCES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\references{
Linacre, J. M. (1994). \emph{Many-Facet Rasch Measurement}.
Chicago: MESA Press.
}

%\author{
%%  ~~who you are~~
%}

\note{
This joint maximum likelihood estimation procedure should be
compatible with Winsteps and Facets software, see also
\url{http://www.rasch.org/software.htm}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For estimating the same class of models with marginal
maximum likelihood estimation see \code{\link{tam.mml}}.
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\examples{
#############################################################################
# EXAMPLE 1: Dichotomous data
#############################################################################
data(data.sim.rasch)
resp <- data.sim.rasch[1:700, seq( 1, 40, len=10)  ]  # subsample
# estimate the Rasch model with JML (function 'tam.jml')
mod1a <- TAM::tam.jml(resp=resp)
summary(mod1a)
itemfit <- TAM::tam.fit(mod1a)$fit.item

# compare results with Rasch model estimated by MML
mod1b <- TAM::tam.mml(resp=resp )

# plot estimated parameters
plot( mod1a$xsi, mod1b$xsi$xsi, pch=16,
    xlab=expression( paste( xi[i], " (JML)" )),
    ylab=expression( paste( xi[i], " (MML)" )),
    main="Item Parameter Estimate Comparison")
lines( c(-5,5), c(-5,5), col="gray" )

# Now, the adjustment pf .05 instead of the default .3 is used.
mod1d <- TAM::tam.jml(resp=resp, adj=.05)
# compare item parameters
round( rbind( mod1a$xsi, mod1d$xsi ), 3 )
  ##          [,1]   [,2]   [,3]   [,4]   [,5]  [,6]  [,7]  [,8]  [,9] [,10]
  ##   [1,] -2.076 -1.743 -1.217 -0.733 -0.338 0.147 0.593 1.158 1.570 2.091
  ##   [2,] -2.105 -1.766 -1.233 -0.746 -0.349 0.139 0.587 1.156 1.574 2.108

# person parameters for persons with a score 0, 5 and 10
pers1 <- data.frame( "score_adj0.3"=mod1a$PersonScore, "theta_adj0.3"=mod1a$theta,
           "score_adj0.05"=mod1d$PersonScore, "theta_adj0.05"=mod1d$theta  )
round( pers1[ c(698, 683, 608), ],3  )
  ##       score_adj0.3 theta_adj0.3 score_adj0.05 theta_adj0.05
  ##   698          0.3       -4.404          0.05        -6.283
  ##   683          5.0       -0.070          5.00        -0.081
  ##   608          9.7        4.315          9.95         6.179

\dontrun{
#*** item fit and person fit statistics
fmod1a <- TAM::tam.jml.fit(mod1a)
head(fmod1a$fit.item)
head(fmod1a$fit.person)

#*** Models in which some item parameters are fixed
xsi.fixed <- cbind( c(1,3,9,10), c(-2, -1.2, 1.6, 2 ) )
mod1e <- TAM::tam.jml( resp=resp, xsi.fixed=xsi.fixed )
summary(mod1e)

#*** Model in which also some person parameters theta are fixed
# fix theta parameters of persons 2, 3, 4 and 33 to values -2.9, ...
theta.fixed <- cbind( c(2,3,4,33), c( -2.9, 4, -2.9, -2.9 ) )
mod1g <- TAM::tam.jml( resp=resp, xsi.fixed=xsi.fixed, theta.fixed=theta.fixed )
# look at estimated results
ind.person <- c( 1:5, 30:33 )
cbind( mod1g$WLE, mod1g$errorWLE )[ind.person,]

#############################################################################
# EXAMPLE 2: Partial credit model
#############################################################################

data(data.gpcm)
# JML estimation
mod2 <- TAM::tam.jml(resp=data.gpcm)
mod2$xsi     # extract item parameters
summary(mod2)
tam.fit(mod2)    # item and person infit/outfit statistic

#############################################################################
# EXAMPLE 3: Facet model estimation using joint maximum likelihood
#            data.ex10; see also Example 10 in tam.mml
#############################################################################

data(data.ex10)
dat <- data.ex10
  ## > head(dat)
  ##  pid rater I0001 I0002 I0003 I0004 I0005
  ##    1     1     0     1     1     0     0
  ##    1     2     1     1     1     1     0
  ##    1     3     1     1     1     0     1
  ##    2     2     1     1     1     0     1
  ##    2     3     1     1     0     1     1

facets <- dat[, "rater", drop=FALSE ] # define facet (rater)
pid <- dat$pid      # define person identifier (a person occurs multiple times)
resp <- dat[, -c(1:2) ]        # item response data
formulaA <- ~ item * rater      # formula

# use MML function only to restructure data and input obtained design matrices
# and processed response data to tam.jml (-> therefore use only 2 iterations)
mod3a <- TAM::tam.mml.mfr( resp=resp, facets=facets, formulaA=formulaA,
             pid=dat$pid,  control=list(maxiter=2) )

# use modified response data mod3a$resp and design matrix mod3a$A
resp1 <- mod3a$resp
# JML
mod3b <- TAM::tam.jml( resp=resp1, A=mod3a$A, control=list(maxiter=200) )

#############################################################################
# EXAMPLE 4: Multi faceted model with some anchored item and person parameters
#############################################################################

data(data.exJ03)
resp <- data.exJ03$resp
X <- data.exJ03$X

#*** (0) preprocess data with TAM::tam.mml.mfr
mod0 <- TAM::tam.mml.mfr( resp=resp, facets=X, pid=X$rater,
                formulaA=~ leader + item + step,
                control=list(maxiter=2) )
summary(mod0)

#*** (1) estimation with tam.jml (no parameter fixings)

# extract processed data and design matrix from tam.mml.mfr
resp1 <- mod0$resp
A1 <- mod0$A
# estimate model with tam.jml
mod1 <- TAM::tam.jml( resp=resp1, A=A1, control=list( Msteps=4, maxiter=100 ) )
summary(mod1)

#*** (2) fix some parameters (persons and items)

# look at indices in mod1$xsi
mod1$xsi
# fix step parameters
xsi.index1 <- cbind( 21:25, c( -2.44, 0.01, -0.15, 0.01,  1.55 ) )
# fix some item parameters of items 1,2,3,6 and 13
xsi.index2 <- cbind( c(1,2,3,6,13), c(-2,-1,-1,-1.32, -1 ) )
xsi.index <- rbind( xsi.index1, xsi.index2 )
# fix some theta parameters of persons 1, 15 and 20
theta.fixed <- cbind(  c(1,15,20), c(0.4, 1, 0 ) )
# estimate model, theta.fixed only works for version=1
mod2 <- TAM::tam.jml( resp=resp1, A=A1, xsi.fixed=xsi.fixed, theta.fixed=theta.fixed,
            control=list( Msteps=4, maxiter=100) )
summary(mod2)
cbind( mod2$WLE, mod2$errorWLE )
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Joint maximum likelihood estimation (JML)}
%% \keyword{summary}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
