%% File Name: tam.np.Rd
%% File Version: 0.12

\name{tam.np}
\alias{tam.np}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Unidimensional Nonparametric Item Response Model
}


\description{
Conducts nonparametric estimation of a unidimensional item response model
for a single group allowing polytomous item responses (Rossi, Wang & Ramsay, 2002).
}


\usage{
tam.np(dat, probs_init=NULL, pweights=NULL, control=list())
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
  \item{control}{
List of control arguments, see \code{\link{tam.mml}}.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
List containing several entries
\item{rprobs}{Item response probabilities}
\item{theta}{Used nodes for approximation of \eqn{\theta} distribution}
\item{n.ik}{Expected counts}
\item{like}{Individual likelihood}
\item{hwt}{Individual posterior}
\item{\dots}{Further values}
}

\references{
Rossi, N., Wang, X., & Ramsay, J. O. (2002). Nonparametric item response function
estimates with the EM algorithm.
\emph{Journal of Educational and Behavioral Statistics, 27}(3), 291-317.
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
mod <- TAM::tam.np( dat )

#** extractor functions for objects of class 'tam.np'
lmod <- IRT.likelihood(mod)
pmod <- IRT.posterior(mod)
rmod <- IRT.irfprob(mod)
emod <- IRT.expectedCounts(mod)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
