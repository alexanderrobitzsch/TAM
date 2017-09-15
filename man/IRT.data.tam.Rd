%% File Name: IRT.data.tam.Rd
%% File Version: 0.14
%% File Last Change: 2017-06-22 17:41:51

\name{IRT.data.tam}

\alias{IRT.data.tam.mml}
\alias{IRT.data.tam.mml.3pl}
\alias{IRT.data.tamaan}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracting Item Response Dataset
}


\description{
Extracts the used data set for models
fitted in \pkg{TAM}. See \code{\link[CDM:IRT.data]{CDM::IRT.data}} 
 for more details.
}


\usage{
\method{IRT.data}{tam.mml}(object, \dots)

\method{IRT.data}{tam.mml.3pl}(object, \dots)

\method{IRT.data}{tamaan}(object, \dots)
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
A dataset with item responses
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
# EXAMPLE 1: Dichotomous data data.sim.rasch
#############################################################################	

data(data.sim.rasch)
dat <- data.sim.rasch

# estimate model
mod1 <- TAM::tam.mml(dat)
# extract dataset (and weights and group if available)
dmod1 <- IRT.data(mod1)
str(dmod1)
}	
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{Expected counts}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
