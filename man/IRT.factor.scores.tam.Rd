%% File Name: IRT.factor.scores.tam.Rd
%% File Version: 0.17

\name{IRT.factor.scores}

\alias{IRT.factor.scores.tam}
\alias{IRT.factor.scores.tam.mml}
\alias{IRT.factor.scores.tam.mml.3pl}
\alias{IRT.factor.scores.tamaan}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracting Factor Scores in \pkg{TAM}
}


\description{
Extracts factor scores for models
fitted in \pkg{TAM}. See \code{\link[CDM:IRT.factor.scores]{CDM::IRT.factor.scores}}
for more details.
}


\usage{
\method{IRT.factor.scores}{tam}(object, type="EAP", \dots)

\method{IRT.factor.scores}{tam.mml}(object, type="EAP", \dots)

\method{IRT.factor.scores}{tam.mml.3pl}(object, type="EAP", \dots)

\method{IRT.factor.scores}{tamaan}(object, type="EAP", \dots)
}



\arguments{
  \item{object}{
Object of class \code{\link{tam}}, \code{\link{tam.mml}},
\code{\link{tam.mml.3pl}} or \code{\link{tamaan}}.
}
\item{type}{Type of factor score to be used. \code{type="EAP"} can be used
    for all classes in \pkg{TAM} while \code{type="WLE"} and
    \code{type="MLE"} can only be used for objects of class
     \code{\link{tam.mml}}. Further arguments to the
     used function \code{\link{tam.wle}} can be specified
     with \code{\dots}.
}
\item{\dots}{Further arguments to be passed}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
See \code{\link[CDM:IRT.factor.scores]{CDM::IRT.factor.scores}}.
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
# EXAMPLE 1: data.sim.rasch - Different factor scores in Rasch model
#############################################################################

data(data.sim.rasch)
# 1PL estimation
mod1 <- TAM::tam.mml(resp=data.sim.rasch)
# EAP
pmod1 <- IRT.factor.scores( mod1 )
# WLE
pmod1 <- IRT.factor.scores( mod1, type="WLE" )
# MLE
pmod1 <- IRT.factor.scores( mod1, type="MLE" )
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Factor scores}
% \keyword{Individual posterior}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
