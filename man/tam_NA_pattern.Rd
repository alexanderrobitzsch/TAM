%% File Name: tam_NA_pattern.Rd
%% File Version: 0.05

\name{tam_NA_pattern}
\alias{tam_NA_pattern}
\alias{tam_01_pattern}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Missing Data Patterns
}

\description{
Determines patterns of missing values or pattern of dichotomous item
responses.
}

\usage{
tam_NA_pattern(x)

tam_01_pattern(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{
Matrix or data frame
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List containing identifiers and indices
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

% \seealso{
% xxxx
% }


\examples{
#############################################################################
# EXAMPLE 1: Missing data patterns
#############################################################################

data(data.sim.rasch.missing, package="TAM")
dat <- data.sim.rasch.missing

res <- TAM::tam_NA_pattern(dat)
str(res)

#############################################################################
# EXAMPLE 2: Item response patterns
#############################################################################

data(data.read, package="sirt")
dat <- data.read

res <- TAM::tam_01_pattern(dat)
str(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{DO statements}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
