%% File Name: IRT.irfprob.tam.Rd
%% File Version: 0.18

\name{IRT.irfprob}

\alias{IRT.irfprob.tam}
\alias{IRT.irfprob.tam.mml}
\alias{IRT.irfprob.tam.mml.3pl}
\alias{IRT.irfprob.tamaan}
\alias{IRT.irfprob.tam.np}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracting Item Response Functions
}


\description{
Extracts item response functions for models
fitted in \pkg{TAM}. See \code{\link[CDM:IRT.irfprob]{CDM::IRT.irfprob}}
 for more details.
}


\usage{
\method{IRT.irfprob}{tam}(object, \dots)

\method{IRT.irfprob}{tam.mml}(object, \dots)

\method{IRT.irfprob}{tam.mml.3pl}(object, \dots)

\method{IRT.irfprob}{tamaan}(object, \dots)

\method{IRT.irfprob}{tam.np}(object, \dots)
}



\arguments{
  \item{object}{
Object of class \code{\link{tam}}, \code{\link{tam.mml}},
\code{\link{tam.mml.3pl}}, \code{\link{tam.np}} or \code{\link{tamaan}}.
}
\item{\dots}{Further arguments to be passed}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
See \code{\link[CDM:IRT.irfprob]{CDM::IRT.irfprob}}.
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}


\examples{
#############################################################################
# EXAMPLE 1: Dichotomous data data.sim.rasch - item response functions
#############################################################################

data(data.sim.rasch)
# 1PL estimation
mod1 <- TAM::tam.mml(resp=data.sim.rasch)
IRT.irfprob(mod1)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Item response functions}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
