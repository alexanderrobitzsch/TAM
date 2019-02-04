%% File Name: doparse.Rd
%% File Version: 0.20

\name{doparse}
\alias{doparse}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Parsing a String with \code{DO} Statements
}

\description{
This function parses a string and expands this string in case of \code{DO}
statements which are shortcuts for writing loops. The statement
\code{DO(n,m,k)} increments an index from \code{n} to \code{m} in steps
of \code{k}. The index in the string \code{model} must be defined
as \code{\%}. For a nested loop within a loop,
the \code{DO2} statement can be used using \code{\%1} and \code{\%2}
as indices. See Examples for hints on the specification. The loop in \code{DO2}
must not be explicitly crossed, e.g. in applications for specifying
covariances or correlations. The formal syntax for \cr
\code{ for (ii in 1:(K-1)){  for (jj in (ii+1):K) { ... } }} \cr
can be written as \code{DO2(1,K-1,1,\%1,K,1
)}. See Example 2.
}

\usage{
doparse(model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A string with \code{DO} or \code{DO2} statements.
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Parsed string in which \code{DO} statements are expanded.
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
This function is also used in \code{\link{lavaanify.IRT}} and
\code{\link{tamaanify}}.
}


\examples{
#############################################################################
# EXAMPLE 1: doparse example
#############################################################################

# define model
model <- "
 # items I1,...,I10 load on G
 DO(1,10,1)
   G=~ lamg\% * I\%
 DOEND
 I2 | 0.75*t1
 v10 > 0 ;
 # The first index loops from 1 to 3 and the second index loops from 1 to 2
 DO2(1,3,1,  1,2,1)
   F\%2=~ a\%2\%1 * A\%2\%1 ;
 DOEND
 # Loop from 1 to 9 with steps of 2
 DO(1,9,2)
   HA1=~ I\%
   I\% | beta\% * t1
 DOEND
 "

# process string
out <- TAM::doparse(model)
cat(out)
  ##    # items I1,...,I10 load on G
  ##       G=~ lamg1 * I1
  ##       G=~ lamg2 * I2
  ##       G=~ lamg3 * I3
  ##       G=~ lamg4 * I4
  ##       G=~ lamg5 * I5
  ##       G=~ lamg6 * I6
  ##       G=~ lamg7 * I7
  ##       G=~ lamg8 * I8
  ##       G=~ lamg9 * I9
  ##       G=~ lamg10 * I10
  ##     I2 | 0.75*t1
  ##     v10 > 0
  ##       F1=~ a11 * A11
  ##       F2=~ a21 * A21
  ##       F1=~ a12 * A12
  ##       F2=~ a22 * A22
  ##       F1=~ a13 * A13
  ##       F2=~ a23 * A23
  ##       HA1=~ I1
  ##       HA1=~ I3
  ##       HA1=~ I5
  ##       HA1=~ I7
  ##       HA1=~ I9
  ##       I1 | beta1 * t1
  ##       I3 | beta3 * t1
  ##       I5 | beta5 * t1
  ##       I7 | beta7 * t1
  ##       I9 | beta9 * t1

#############################################################################
# EXAMPLE 2: doparse with nested loop example
#############################################################################

# define model
model <- "
 DO(1,4,1)
   G=~ lamg\% * I\%
 DOEND
 # specify some correlated residuals
 DO2(1,3,1,\%1,4,1)
   I\%1 ~~ I\%2
 DOEND
 "
# process string
out <- TAM::doparse(model)
cat(out)
  ##       G=~ lamg1 * I1
  ##       G=~ lamg2 * I2
  ##       G=~ lamg3 * I3
  ##       G=~ lamg4 * I4
  ##     # specify some correlated residuals
  ##       I1 ~~ I2
  ##       I1 ~~ I3
  ##       I1 ~~ I4
  ##       I2 ~~ I3
  ##       I2 ~~ I4
  ##       I3 ~~ I4
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{DO statements}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
