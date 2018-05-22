%% File Name: plotDevianceTAM.Rd
%% File Version: 2.16

\name{plotDevianceTAM}
\alias{plotDevianceTAM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Deviance Plot for \pkg{TAM} Objects
}
\description{
Plots the deviance change in every iteration.
}

\usage{
plotDevianceTAM(tam.obj, omitUntil=1, reverse=TRUE, change=TRUE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tam.obj}{
Object of class \code{tam.mml}, \code{tam.mml.2pl} or \code{tam.mml.mfr}.
}
  \item{omitUntil}{
An optional value indicating number of iterations to be omitted for
plotting.
}
  \item{reverse}{
A logical indicating whether the deviance change should be
multiplied by minus 1. The default is \code{TRUE}.
}
\item{change}{An optional logical indicating whether deviance change
or the deviance should be plotted.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Martin Hecht, Sebastian Weirich, Alexander Robitzsch
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
#############################################################################
# EXAMPLE 1: deviance plot dichotomous data
#############################################################################
data(data.sim.rasch)

# 2PL model
mod1 <- TAM::tam.mml.2pl(resp=data.sim.rasch )
# plot deviance change
plotDevianceTAM( mod1 )
# plot deviance
plotDevianceTAM( mod1, change=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
