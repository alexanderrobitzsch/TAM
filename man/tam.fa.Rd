%% File Name: tam.fa.Rd
%% File Version: 2.21

\name{tam.fa}
\alias{tam.fa}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Bifactor Model and Exploratory Factor Analysis
}


\description{
Estimates the bifactor model and exploratory factor analysis with
marginal maximum likelihood estimation.

This function is simply a wrapper to \code{\link{tam.mml}} or
\code{\link{tam.mml.2pl}}.
}



\usage{
tam.fa(resp, irtmodel, dims=NULL, nfactors=NULL, pid=NULL,
    pweights=NULL, verbose=TRUE, control=list())
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ARGUMENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\arguments{
\item{resp}{
     Data frame with polytomous item responses \eqn{k=0,...,K}.
     Missing responses must be declared as \code{NA}.
}
  \item{irtmodel}{
A string which defines the IRT model to be estimated. Options
are \code{"efa"} (exploratory factor analysis), \code{"bifactor1"} (Rasch
testlet model in case of dichotomous data; Wang & Wilson, 2005;
for polytomous data it assumes item slopes of 1) and \code{"bifactor2"}
(bifactor model).
See Details for more information.
}
\item{dims}{
A numeric or string vector which only applies in case of
\code{irtmodel="bifactor1"} or \code{irtmodel="bifactor2"}.
Different entries in the vector indicate different dimensions of items
which should load on the nested factor. If items should only load
on the general factor, then an \code{NA} must be specified.
}
\item{nfactors}{
A numerical value which indicates the number of factors in
exploratory factor analysis.
}
  \item{pid}{
     An optional vector of person identifiers
}
  \item{pweights}{
     An optional vector of person weights
}
\item{verbose}{Logical indicating whether output should
be printed during iterations. This argument replaces \code{control$progress}.
}
  \item{control}{
See \code{\link{tam.mml}} for more details. Note that the default is
Quasi Monte Carlo integration with 1500 nodes (\code{snodes=1500},
\code{QMC=TRUE}).
}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DETAILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\details{
The exploratory factor analysis (\code{irtmodel="efa"}
is estimated using an echelon form of the loading matrix and uncorrelated factors.
The obtained standardized loading matrix is rotated using oblimin rotation.
In addition, a Schmid-Leimann transformation (see Revelle & Zinbarg, 2009)
is employed.

The bifactor model (\code{irtmodel="bifactor2"}; Reise 2012)
for dichotomous responses is defined as
\deqn{logit P(X_{pi}=1 | \theta_{pg}, u_{p1}, \ldots, u_{pD} )=
    a_{i0} \theta_{pg} + a_{i1} u_{pd(i) } }
Items load on the general factor \eqn{\theta_{pg}} and a specific (nested)
factor \eqn{u_{pd(i) }}. All factors are assumed to be uncorrelated.

In the Rasch testlet model (\code{irtmodel="bifactor1"}),
all item slopes are set to 1 and variances are
estimated.

For polytomous data, the generalized partial credit model is used. The loading
structure is defined in the same way as for dichotomous data.
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VALUES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\value{
The same list entries as in \code{\link{tam.mml}} but in addition the
following statistics are included:
\item{B.stand}{Standardized factor loadings of the bifactor model or the
exploratory factor analysis.}
\item{B.SL}{ In case of exploratory factor analysis (\code{irtmodel="efa"}),
loadings form the Schmid-Leimann solution of the \pkg{psych} package.}
\item{efa.oblimin}{Output from oblimin rotation in exploratory
   factor analysis which is produced by the \pkg{GPArotation} package}
\item{meas}{Vector of dimensionality and reliability statistics.
   Included are the ECV measure (explained common variation;
   Reise, Moore & Haviland, 2010; Reise, 2012),
   \eqn{\omega_t} (Omega Total), \eqn{\omega_a} (Omega asymptotic)
   and \eqn{\omega_h} (Omega hierarchical) (Revelle & Zinbarg, 2009).
   The reliability of the sum score based on
   the bifactor model for dichotomous item responses is also
   included (Green & Yang, 2009).
  }
}

%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REFERENCES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\references{
Green, S. B., & Yang, Y. (2009). Reliability of summed item
scores using structural equation modeling: An alternative to
coefficient alpha. \emph{Psychometrika, 74}, 155-167.

Reise, S. P. (2012). The rediscovery of bifactor measurement models.
\emph{Multivariate Behavioral Research, 47}, 667-696.

Reise, S. P., Moore, T. M., & Haviland, M. G.  (2010).
Bifactor models and rotations: Exploring the extent to which
multidimensional data yield univocal scale scores,
\emph{Journal of Personality Assessment, 92}, 544-559.

Revelle, W., & Zinbarg, R. E. (2009).
Coefficients alpha, beta, omega and the glb: Comments on Sijtsma.
\emph{Psychometrika, 74}, 145-154.

Wang, W.-C., & Wilson, M. (2005). The Rasch testlet model.
\emph{Applied Psychological Measurement, 29}, 126-149.
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{
For more details see \code{\link{tam.mml}} because \code{tam.fa} is just
a wrapper for \code{\link{tam.mml.2pl}} and \code{\link{tam.mml}}.

\code{\link{logLik.tam}}, \code{\link{anova.tam}}
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Dataset reading from sirt package
#############################################################################

data(data.read,package="sirt")
resp <- data.read

#***
# Model 1a: Exploratory factor analysis with 2 factors
mod1a <- TAM::tam.fa( resp=resp, irtmodel="efa", nfactors=2  )
summary(mod1a)
# varimax rotation
stats::varimax(mod1a$B.stand)
# promax rotation
stats::promax(mod1a$B.stand)
# more rotations are included in the GPArotation package
library(GPArotation)
# geomin rotation oblique
GPArotation::geominQ( mod1a$B.stand )
# quartimin rotation
GPArotation::quartimin( mod1a$B.stand )

#***
# Model 1b: Rasch testlet model with 3 testlets
dims <- substring( colnames(resp),1,1 )     # define dimensions
mod1b <- TAM::tam.fa( resp=resp, irtmodel="bifactor1", dims=dims )
summary(mod1b)

#***
# Model 1c: Bifactor model
mod1c <- TAM::tam.fa( resp=resp, irtmodel="bifactor2", dims=dims )
summary(mod1c)

#***
# Model 1d: reestimate Model 1c but assume that items 3 and 5 do not load on
#           specific factors
dims1 <- dims
dims1[c(3,5)] <- NA
mod1d <- TAM::tam.fa( resp=resp, irtmodel="bifactor2", dims=dims1 )
summary(mod1d)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Exploratory factor analysis}
%% \keyword{Testlet model}
%% \keyword{Bifactor model}
