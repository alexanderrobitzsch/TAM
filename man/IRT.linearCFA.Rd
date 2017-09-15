%% File Name: IRT.linearCFA.Rd
%% File Version: 0.12
%% File Last Change: 2017-06-22 17:41:51

\name{IRT.linearCFA}

\alias{IRT.linearCFA}
\alias{summary.IRT.linearCFA}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Linear Approximation of a Confirmatory Factor Analysis
}

\description{
This function approximates a fitted item response model by a linear
confirmatory factor analysis. I.e., given item response functions, the
expectation \eqn{E(X_i | \theta_1 , \ldots , \theta_D)} is
linearly approximated by \eqn{a_{i1} \theta _1 + \ldots + a_{iD} \theta_D}. 
See Vermunt and Magidson (2005) for details.
}


\usage{
IRT.linearCFA( object , group=1)
	
\method{summary}{IRT.linearCFA}(object,  \dots)
}
%- maybe also 'usage' for other objects documented here.
% IRT.WrightMap(object, prob.lvl= .5, type="PV", \dots)


\arguments{
  \item{object}{
Fitted item response model for which the \code{IRT.expectedCounts}
method is defined.
}
\item{group}{Group identifier which defines the selected group.}
\item{\dots}{Further arguments to be passed.}
}

%\details{
%xxx
%}


\value{
A list with following entries
\item{loadings}{Data frame with factor loadings. \code{Mlat} and
\code{SDlat} denote the model-implied item mean and standard deviation.
The values \code{ResidVar} and \code{h2} denote residual variances
and item communality.
}
\item{stand.loadings}{Data frame with standardized factor loadings.}
\item{M.trait}{Mean of factors}
\item{SD.trait}{Standard deviations of factors}
}

\references{
Vermunt, J. K., & Magidson, J. (2005). Factor Analysis with categorical 
indicators: A comparison between traditional and latent class approaches. 
In A. Van der Ark, M.A. Croon & K. Sijtsma (Eds.), 
\emph{New Developments in Categorical Data Analysis for the Social and 
Behavioral Sciences} (pp. 41-62). Mahwah: Erlbaum
}

%\author{
%}

% \note{		
% See \url{http://wrightmap.org/post/107431190622/wrightmap-multifaceted-models}
% for creating Wright maps for multi-faceted models.
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{tam.fa}} for confirmatory factor analysis in \pkg{TAM}. 
}

\examples{
\dontrun{
library(lavaan)	
	
#############################################################################
# EXAMPLE 1: Two-dimensional confirmatory factor analysis data.Students
#############################################################################

data(data.Students, package="CDM")
# select variables
vars <- scan(nlines=1 , what="character")
    sc1 sc2 sc3 sc4 mj1 mj2 mj3 mj4
dat <- data.Students[ , vars]

# define Q-matrix
Q <- matrix( 0 , nrow=8 , ncol=2 )
Q[1:4,1] <- Q[5:8,2] <- 1

#*** Model 1: Two-dimensional 2PL model
mod1 <- TAM::tam.mml.2pl( dat , Q=Q  , control=list( nodes=seq(-4,4,len=12) ) )
summary(mod1)

# linear approximation CFA
cfa1 <- TAM::IRT.linearCFA(mod1)
summary(cfa1)

# linear CFA in lavaan package
lavmodel <- "
    sc =~ sc1+sc2+sc3+sc4
    mj =~ mj1+mj2+mj3+mj4
    sc1 ~ 1
    sc ~~ mj
    "
mod1b <- lavaan::sem( lavmodel , data = dat , missing="fiml" , std.lv=TRUE)    
summary(mod1b , standardized=TRUE , fit.measures=TRUE )

#############################################################################
# EXAMPLE 2: Unidimensional confirmatory factor analysis data.Students
#############################################################################

data(data.Students, package="CDM")
# select variables
vars <- scan(nlines=1 , what="character")
    sc1 sc2 sc3 sc4
dat <- data.Students[ , vars]

#*** Model 1: 2PL model
mod1 <- TAM::tam.mml.2pl( dat )
summary(mod1)

# linear approximation CFA
cfa1 <- TAM::IRT.linearCFA(mod1)
summary(cfa1)

# linear CFA
lavmodel <- "
    sc =~ sc1+sc2+sc3+sc4
    "
mod1b <- lavaan::sem( lavmodel , data = dat , missing="fiml" , std.lv=TRUE)    
summary(mod1b , standardized=TRUE , fit.measures=TRUE )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Confirmatory factor analysis}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
