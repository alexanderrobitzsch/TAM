%% File Name: tam.personfit.Rd
%% File Version: 0.07

\name{tam.personfit}
\alias{tam.personfit}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Person Outfit and Infit Statistics
}

\description{
Computes person outfit and infit statistics.
}

\usage{
tam.personfit(tamobj)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tamobj}{
Fitted object in \pkg{TAM}
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Data frame containing person outfit and infit statistics
}

%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{tam.fit}} and \code{\link{msq.itemfit}} for item fit
statistics.
}


\examples{
#############################################################################
# EXAMPLE 1: Person fit dichotomous data
#############################################################################

data(data.sim.rasch, package="TAM")
resp <- data.sim.rasch

#*** estimate Rasch model
mod1 <- TAM::tam.mml(resp=resp)
summary(mod1)

#*** compute person fit statistics
fmod1 <- TAM::tam.personfit(mod1)
head(fmod1)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
