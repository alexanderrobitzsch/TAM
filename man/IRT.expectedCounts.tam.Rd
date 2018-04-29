%% File Name: IRT.expectedCounts.tam.Rd
%% File Version: 0.13

\name{IRT.expectedCounts}

\alias{IRT.expectedCounts.tam}
\alias{IRT.expectedCounts.tam.mml}
\alias{IRT.expectedCounts.tam.mml.3pl}
\alias{IRT.expectedCounts.tamaan}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracting Expected Counts
}


\description{
Extracts expected counts for models
fitted in \pkg{TAM}. See \code{\link[CDM:IRT.expectedCounts]{CDM::IRT.expectedCounts}}
for more details.
}


\usage{
\method{IRT.expectedCounts}{tam}(object, \dots)

\method{IRT.expectedCounts}{tam.mml}(object, \dots)

\method{IRT.expectedCounts}{tam.mml.3pl}(object, \dots)

\method{IRT.expectedCounts}{tamaan}(object, \dots)
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
See \code{\link[CDM:IRT.expectedCounts]{CDM::IRT.expectedCounts}}.
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
\dontrun{
#############################################################################
# EXAMPLE 1: Dichotomous data data.sim.rasch - extract expected counts
#############################################################################

data(data.sim.rasch)
# 1PL estimation
mod1 <- TAM::tam.mml(resp=data.sim.rasch)
# extract expected counts
IRT.expectedCounts(mod1)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Expected counts}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
