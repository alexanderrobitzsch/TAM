%% File Name: tampv2datalist.Rd
%% File Version: 0.16
%% File Last Change: 2017-09-20 16:46:21

\name{tampv2datalist}
\alias{tampv2datalist}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Conversion of Plausible Value Object into Datalist
}

\description{
Converts a \code{\link{tam.pv}} object and a matrix of covariates
into a list of multiply imputed datasets. This list can be conveniently
analyzed by \R packages such as \pkg{semTools}, \pkg{Zelig}, \pkg{mice}
or \pkg{BIFIEsurvey}.
}

\usage{
tampv2datalist(tam.pv.object, pvnames = NULL, Y = NULL, Y.pid = "pid", as_mids=FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tam.pv.object}{
Generated \code{\link{tam.pv}} object
}
  \item{pvnames}{
Variable names of generated plausible values
}
  \item{Y}{
Matrix with covariates
}
  \item{Y.pid}{
Person identifier in \code{Y} matrix. It is not required that a person
identifier is provided. In this case, the merge of the datasets will
be conducted as for \code{rbind}.
}
\item{as_mids}{Logical indicating whether the datalist
  should be converted into an object of class \code{mids} for
  analysis in the \pkg{mice} package. This functionality uses the
  the function \code{\link[miceadds:datalist2mids]{miceadds::datalist2mids}}.
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
List of multiply imputed datasets or an \code{mids} object
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
For examples see \code{\link{tam.pv}}.
}

%\examples{
% ####
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
