%% File Name: lavaanify.IRT.Rd
%% File Version: 1.30
%% File Last Change: 2017-06-22 17:41:51

\name{lavaanify.IRT}
\alias{lavaanify.IRT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Slight Extension of the \code{lavaan} Syntax, with Focus on Item Response Models 
}

\description{
This functions slightly extends the \code{lavaan}
syntax implemented in the \pkg{lavaan} package
(see \code{\link[lavaan:lavaanify]{lavaan::lavaanify}}).

Guessing and slipping parameters can be specified 
by using the operators \code{?=g1} and \code{?=s1},
respectively. 

The operator \code{__} can be used for a convenient
specification for groups of items. For example, \code{I1__I5} refers
to items \code{I1,...,I5}. The operator \code{__} can also be used for
item labels (see Example 2). 

Nonlinear terms can also be specified for loadings (\code{=~}) and
regressions (\code{~}) (see Example 3).

It is also possible to construct the syntax using a loop by making use
of the \code{DO} statement, see \code{\link{doparse}} for specification.

The operators \code{MEASERR1} and \code{MEASERR0} can be used for
model specification for variables which contains known measurement
error (see Example 6). While \code{MEASERR1} can be used for endogeneous
variables, \code{MEASERR0} provides the specification for exogeneous variables.

}

\usage{
lavaanify.IRT(lavmodel, items=NULL, data=NULL, include.residuals=TRUE,
    doparse=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lavmodel}{
A model in \code{lavaan} syntax plus the additional
operators \code{?=g1}, \code{?=s1}, \code{__} and nonlinear terms.
}
\item{items}{Optional vector of item names}
\item{data}{Optional data frame with item responses}
\item{include.residuals}{Optional logical indicating whether
       residual variances should be processed such that they
       are freely estimated.}
\item{doparse}{Optional logical indicating whether \code{lavmodel}
should be parsed for \code{DO} statements.} 
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A list with following entries	
\item{lavpartable}{A \code{lavaan} parameter table}
\item{lavaan.syntax}{Processed syntax for \pkg{lavaan} package}
\item{nonlin_factors}{Data frame with renamed and original nonlinear
factor specifications}
\item{nonlin_syntable}{Data frame with original and modified 
syntax if nonlinear factors are used.}
}

%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%Alexander Robitzsch
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[lavaan:lavaanify]{lavaan::lavaanify}}


See \code{\link[sirt:tam2mirt]{sirt::tam2mirt}} for
converting objects of class \code{tam} into \code{mirt}
objects.

See \code{\link[sirt:lavaan2mirt]{sirt::lavaan2mirt}} 
for estimating models in the \pkg{mirt} package using \code{lavaan} syntax.

See \code{\link{doparse}} for the \code{DO} and \code{DO2} statements.
}

\examples{
library(lavaan)	
	
#############################################################################
# EXAMPLE 1: lavaan syntax with guessing and slipping parameters
#############################################################################

# define model in lavaan
lavmodel <- "
    F =~ A1+c*A2+A3+A4
    # define slipping parameters for A1 and A2
    A1 + A2 ?= s1
    # joint guessing parameter for A1 and A2
    A1+A2 ?= c1*g1
    A3 | 0.75*t1
    # fix guessing parameter to .25 and 
    # slipping parameter to .01 for item A3
    A3 ?= .25*g1+.01*s1
    A4 ?= c2*g1
    A1 | a*t1
    A2 | b*t1
      "

# process lavaan syntax
lavpartable <- TAM::lavaanify.IRT(lavmodel)$lavpartable
  ##     id lhs op rhs user group free ustart exo label eq.id unco
  ##  1   1   F =~  A1    1     1    1     NA   0           0    1
  ##  2   2   F =~  A2    1     1    2     NA   0     c     0    2
  ##  3   3   F =~  A3    1     1    3     NA   0           0    3
  ##  4   4   F =~  A4    1     1    4     NA   0           0    4
  ##  5   5  A3  |  t1    1     1    0   0.75   0           0    0
  ##  6   6  A1  |  t1    1     1    5     NA   0     a     0    5
  ##  7   7  A2  |  t1    1     1    6     NA   0     b     0    6
  ##  8   8  A1 ?=  s1    1     1    7     NA   0           0    7
  ##  9   9  A2 ?=  s1    1     1    8     NA   0           0    8
  ##  10 10  A1 ?=  g1    1     1    9     NA   0    c1     1    9
  ##  11 11  A2 ?=  g1    1     1    9     NA   0    c1     1   10
  ##  12 12  A3 ?=  g1    1     1    0   0.25   0           0    0
  ##  13 13  A3 ?=  s1    1     1    0   0.01   0           0    0
  ##  14 14  A4 ?=  g1    1     1   10     NA   0    c2     0   11

\dontrun{
#############################################################################
# EXAMPLE 2: Usage of "__" and "?=" operators
#############################################################################  

library(sirt)
data(data.read, package="sirt")
dat <- data.read
items <- colnames(dat)

lavmodel <- "
   F1 =~ A1+A2+ A3+lam4*A4
   # equal item loadings for items B1 to B4
   F2 =~ lam5*B1__B4
   # different labelled item loadings of items C1 to C4
   F3 =~ lam9__lam12*C1__C4
   # item intercepts
   B1__B2 | -0.5*t1
   B3__C1 | int6*t1
   # guessing parameters
   C1__C3 ?= g1
   C4 + B1__B3 ?= 0.2*g1
   # slipping parameters
   A1__B1 + B3__C2 ?= slip1*s1
   # residual variances
   B1__B3 ~~ errB*B1__B3 
   A2__A4 ~~ erra1__erra3*A2__A4
    " 
lav2 <- TAM::lavaanify.IRT( lavmodel , data=dat)
lav2$lavpartable
cat( lav2$lavaan.syntax )

#** simplified example
lavmodel <- "
   F1 =~ A1+lam4*A2+A3+lam4*A4
   F2 =~ lam5__lam8*B1__B4
   F1 ~~ F2
   F1 ~~ 1*F1
   F2 ~~ 1*F2
    "   
lav3 <- TAM::lavaanify.IRT( lavmodel , data=dat)
lav3$lavpartable
cat( lav3$lavaan.syntax )

#############################################################################
# EXAMPLE 3: Nonlinear terms
#############################################################################

#*** define items
items <- paste0("I",1:12)

#*** define lavaan model
lavmodel <- "
   F1 =~ I1__I5
   F2 =~ I6__I9
   F3 =~ I10__I12
   # I3, I4 and I7 load on interaction of F1 and F2
   I(F1*F2) =~ a*I3+a*I4
   I(F1*F2) =~ I7
   # I3 and I5 load on squared factor F1
   I(F1^2) =~ I3 + I5
   # I1 regression on B spline version of factor F1
   I( bs(F1,4) ) =~ I1
   F2 ~ F1 + b*I(F1^2) + I(F1>0)
   F3 ~ F1 + F2 + 1.4*I(F1*F2) + b*I(F1^2) + I(F2^2 )
   # F3 ~ F2 + I(F2^2)      # this line is ignored in the lavaan model
   F1 ~~ 1*F1
    "   
    
#*** process lavaan syntax    
lav3 <- TAM::lavaanify.IRT( lavmodel , items=items)

#*** inspect results
lav3$lavpartable
cat( lav3$lavaan.syntax )
lav3$nonlin_syntable
lav3$nonlin_factors

#############################################################################
# EXAMPLE 4: Using lavaanify.IRT for estimation with lavaan
#############################################################################

data(data.big5, package="sirt") 
# extract first 10 openness items
items <- which( substring( colnames(data.big5) , 1 , 1 ) == "O"  )[1:10]
dat <- as.data.frame( data.big5[ , items ] )
  ##   > colnames(dat)
  ##    [1] "O3"  "O8"  "O13" "O18" "O23" "O28" "O33" "O38" "O43" "O48"
apply(dat,2,var)  # variances

#*** Model 1: Confirmatory factor analysis with one factor
lavmodel <- "
   O =~ O3__O48   # convenient syntax for defining the factor for all items
   O ~~ 1*O
   "
# process lavaan syntax   
res <- TAM::lavaanify.IRT( lavmodel , data = dat )
# estimate lavaan model 
mod1 <- lavaan::lavaan( model= res$lavaan.syntax , data=dat)
summary(mod1 , standardized=TRUE , fit.measures = TRUE , rsquare=TRUE )
}

#############################################################################
# EXAMPLE 5: lavaanify.IRT with do statements
#############################################################################

lavmodel <- "
  DO(1,6,1)
    F =~ I\%
  DOEND
  DO(1,5,2)
    A =~ I\%
  DOEND  
  DO(2,6,2)
    B =~ I\%
  DOEND
  F ~~ 1*F
  A ~~ 1*A
  B ~~ 1*B
  F ~~ 0*A
  F ~~ 0*B
  A ~~ 0*B
   "
res <- TAM::lavaanify.IRT( lavmodel , items = paste("I",1:6) )   
cat(res$lavaan.syntax)

#############################################################################
# EXAMPLE 6: Single indicator models with measurement error (MEASERR operator)
#############################################################################

# define lavaan model
lavmodel <- "
  ytrue ~ xtrue + z
  # endogeneous variable error-prone y with error variance .20
  MEASERR1(ytrue,y,.20)
  # exogeneous variable error-prone x with error variance .35  
  MEASERR0(xtrue,x,.35)
  ytrue ~~ ytrue
    "  
# observed items
items <- c("y","x","z")
# lavaanify
res <- TAM::lavaanify.IRT( lavmodel , items )
cat(res$lavaan.syntax)
  ##   > cat(res$lavaan.syntax)
  ##   ytrue~xtrue
  ##   ytrue~z
  ##   ytrue=~1*y
  ##   y~~0.2*y
  ##   xtrue=~1*x
  ##   x~~0.35*x
  ##   xtrue~~xtrue
  ##   ytrue~~ytrue
  ##   z~~z 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{lavaan}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
