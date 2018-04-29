%% File Name: IRT.frequencies.tam.Rd
%% File Version: 0.17

\name{IRT.frequencies.tam}

\alias{IRT.frequencies.tam.mml}
\alias{IRT.frequencies.tam.mml.3pl}
\alias{IRT.frequencies.tamaan}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Observed and Expected Frequencies for Univariate and Bivariate Distributions
}


\description{
Computes observed and expected frequencies for univariate and bivariate distributions
for models fitted in \pkg{TAM}. See \code{\link[CDM:IRT.frequencies]{CDM::IRT.frequencies}}
 for more details.
}


\usage{
\method{IRT.frequencies}{tam.mml}(object, \dots)

\method{IRT.frequencies}{tam.mml.3pl}(object, \dots)

\method{IRT.frequencies}{tamaan}(object, \dots)
}



\arguments{
  \item{object}{
Object of class \code{\link{tam}}, \code{\link{tam.mml}},
\code{\link{tam.mml.3pl}} or \code{\link{tamaan}}.
}
\item{\dots}{Further arguments to be passed}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
See \code{\link[CDM:IRT.frequencies]{CDM::IRT.frequencies}}.
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
\code{\link[CDM:IRT.frequencies]{CDM::IRT.frequencies}}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Dichotomous data data.sim.rasch
#############################################################################

data(data.sim.rasch)
dat <- data.sim.rasch

# estimate model
mod1 <- TAM::tam.mml(dat)
# compute observed and expected frequencies
fmod1 <- IRT.frequencies(mod1)
str(fmod1)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{Expected counts}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
