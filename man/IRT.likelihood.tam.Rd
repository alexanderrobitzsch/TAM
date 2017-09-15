%% File Name: IRT.likelihood.tam.Rd
%% File Version: 0.16
%% File Last Change: 2017-06-22 17:41:51

\name{IRT.likelihood}

\alias{IRT.likelihood.tam}
\alias{IRT.likelihood.tam.mml}
\alias{IRT.likelihood.tam.mml.3pl}
\alias{IRT.likelihood.tamaan}
\alias{IRT.likelihood.tam.latreg}

\alias{IRT.posterior.tam}
\alias{IRT.posterior.tam.mml}
\alias{IRT.posterior.tam.mml.3pl}
\alias{IRT.posterior.tamaan}
\alias{IRT.posterior.tam.latreg}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracting Individual Likelihood and Individual Posterior
}


\description{
Extracts individual likelihood and posterior for models
fitted in \pkg{TAM}. See \code{\link[CDM:IRT.likelihood]{CDM::IRT.likelihood}} 
for more details.
}


\usage{
\method{IRT.likelihood}{tam}(object, \dots)
\method{IRT.posterior}{tam}(object, \dots)

\method{IRT.likelihood}{tam.mml}(object, \dots)
\method{IRT.posterior}{tam.mml}(object, \dots)

\method{IRT.likelihood}{tam.mml.3pl}(object, \dots)
\method{IRT.posterior}{tam.mml.3pl}(object, \dots)

\method{IRT.likelihood}{tamaan}(object, \dots)
\method{IRT.posterior}{tamaan}(object, \dots)

\method{IRT.likelihood}{tam.latreg}(object, \dots)
\method{IRT.posterior}{tam.latreg}(object, \dots)
}



\arguments{
  \item{object}{
Object of class \code{\link{tam}}, \code{\link{tam.mml}},
\code{\link{tam.mml.3pl}}, \code{\link{tamaan}}
or \code{\link{tam.latreg}}.
}
\item{\dots}{Further arguments to be passed}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
See \code{\link[CDM:IRT.likelihood]{CDM::IRT.likelihood}}.
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
# EXAMPLE 1: Dichotomous data data.sim.rasch - extracting likelihood/posterior
#############################################################################
	
data(data.sim.rasch)
# 1PL estimation
mod1 <- TAM::tam.mml(resp=data.sim.rasch)
lmod1 <- IRT.likelihood(mod1)
str(lmod1)
pmod1 <- IRT.posterior(mod1)
str(pmod1)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Individual likelihood}
\keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
