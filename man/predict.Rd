%% File Name: predict.Rd
%% File Version: 0.19

\name{predict}

\alias{predict.tam.mml}
\alias{predict.tam.mml.3pl}
\alias{predict.tamaan}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Expected Values and Predicted Probabilities for Fitted \pkg{TAM} Models
}


\description{
Extracts predicted values from the posterior distribution for models
fitted in \pkg{TAM}. 

See \code{\link[CDM:predict]{CDM::predict}} 
for more details.
}


\usage{
\method{predict}{tam.mml}(object, \dots)

\method{predict}{tam.mml.3pl}(object, \dots)

\method{predict}{tamaan}(object, \dots)
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
List with entries for predicted values (expectations and probabilities)
for each person and each item.

See \code{\link[CDM:predict]{predict}} (\pkg{CDM}).
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
# EXAMPLE 1: Dichotomous data sim.rasch - predict method
#############################################################################
	
data(data.sim.rasch)
# 1PL estimation
mod1 <- TAM::tam.mml(resp=data.sim.rasch)
# predict method
prmod1 <- IRT.predict(mod1, data.sim.rasch)
str(prmod1)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Expected counts}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
