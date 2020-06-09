%% File Name: tam_downcode.Rd
%% File Version: 0.04

\name{tam_downcode}
\alias{tam_downcode}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Downcoding an Item Response Dataset
}

\description{
Recodes item categories in a data frame such that each item has values
\eqn{0,1,\ldots,K_i}.
}

\usage{
tam_downcode(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{dat}{
Data frame containing item responses
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List with following entries
\item{dat}{Recoded dataset}
\item{rec}{Recoding table}
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



\examples{
#############################################################################
# EXAMPLE 1: Downcoding in a toy example
#############################################################################

#-- simulate some toy data
set.seed(989)
# values to be sampled
vals <- c(NA, 0:6)
# number of persons and items
N <- 10; I <- 5
dat <- as.data.frame(matrix(NA, nrow=N, ncol=I))
colnames(dat) <- paste0("I",1:I)
for (ii in 1L:I){
    dat[,ii] <- sample(vals, size=N, replace=TRUE)
}

#-- apply downcoding
res <- TAM::tam_downcode(dat)
dat <- res$dat   # extract downcoded dataset
rec <- res$rec   # extract recoded table
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Confirmatory factor analysis}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
