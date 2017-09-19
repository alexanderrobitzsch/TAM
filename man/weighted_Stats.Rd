%% File Name: weighted_Stats.Rd
%% File Version: 1.16
%% File Last Change: 2017-09-19 15:36:28

\name{weighted_Stats}
\alias{weighted_Stats}
\alias{weighted_var}
\alias{weighted_sd}
\alias{weighted_mean}
\alias{weighted_skewness}
\alias{weighted_curtosis}
\alias{weighted_quantile}
\alias{weighted_table}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Descriptive Statistics for Weighted Data
}
\description{
Some descriptive statistics for weighted data: variance,
standard deviation, means, skewness, excess curtosis, quantiles and
frequency tables. Missing values are automatically removed from
the data.
}
\usage{
weighted_mean(x, w = rep(1, length(x)), select = NULL  )

weighted_var(x, w = rep(1, length(x)), method = "unbiased", select = NULL )

weighted_sd(x, w = rep(1, length(x)), method = "unbiased", select = NULL )

weighted_skewness( x , w = rep(1,length(x)), select = NULL  )

weighted_curtosis( x , w = rep(1,length(x)), select = NULL  )

weighted_quantile( x , w=rep(1,length(x)), probs=seq(0,1,.25), type=NULL, select = NULL )

weighted_table( x , w = NULL, props=FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A numeric vector. For \code{weighted_table}, a matrix with
two columns can be used as input for cross-tabulation.
}
  \item{w}{
Optional vector of sample weights
}
\item{select}{Vector referring to selected cases}
  \item{method}{
Computation method (can be \code{"unbiased"} or \code{"ML"})), see
\code{\link[stats:cov.wt]{stats::cov.wt}}
}
\item{probs}{Vector with probabilities}
\item{type}{Quantile type. For unweighted data, quantile types 6 and
7 can be used (see
\code{\link[stats:quantile]{stats::quantile}}). 
For weighted data, the quantile type \code{"i/n"}
is used (see \code{Hmisc::wtd.quantile})).
}
\item{props}{Logical indicating whether relative or abosolute
frequencies should be calculated.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Numeric value
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
See \code{\link[stats:weighted.mean]{stats::weighted.mean}} for 
computing a weighted mean.

See \code{\link[stats:var]{stats::var}} for computing
unweighted variances.

See \code{\link[stats:quantile]{stats::quantile}} and 
\code{Hmisc::wtd.quantile}) for quantiles.
}

\examples{
#############################################################################
# EXAMPLE 1: Toy example for weighted_var function
#############################################################################	

set.seed(9897)
# simulate data
N <- 10
x <- stats::rnorm(N)
w <- stats::runif(N)

#---- variance

# use weighted_var
weighted_var( x=x, w=w )
# use cov.wt
stats::cov.wt( data.frame(x=x) , w=w )$cov[1,1]
\dontrun{
# use wtd.var from Hmisc package
Hmisc::wtd.var(x=x , weights = w , normwt=TRUE , method="unbiased")

#---- standard deviation
weighted_sd( x = x , w=w )

#---- mean
weighted_mean( x = x , w=w )
stats::weighted.mean( x=x , w=w )

#---- weighted quantiles for unweighted data
pvec <- c(.23 , .53 , .78 , .99 )  # choose probabilities
type <- 7

# quantiles for unweighted data
weighted_quantile( x , probs = pvec , type=type)
quantile( x , probs = pvec , type=type)
Hmisc::wtd.quantile(x,probs=pvec, type=type)

# quantiles for weighted data
pvec <- c(.23 , .53 , .78 , .99 )  # probabilities
weighted_quantile( x , w=w , probs = pvec)
Hmisc::wtd.quantile(x, weights=w , probs=pvec)

#--- weighted skewness and curtosis
weighted_skewness(x=x, w=w)
weighted_curtosis(x=x, w=w)

#############################################################################
# EXAMPLE 2: Descriptive statistics normally distributed data
#############################################################################

# simulate some normally distributed data
set.seed(7768)
x <- stats::rnorm( 10000 , mean=1.7, sd=1.2)
# some statistics
weighted_mean(x=x)
weighted_sd(x=x)
weighted_skewness(x=x)
weighted_curtosis(x=x)

#############################################################################
# EXAMPLE 3: Frequency tables
#############################################################################

#*****
# simulate data for weighted frequency tables
y <- scan()
   1 0  1 1   1 2   1 3    1 4
   2 0  2 1   2 2   2 3    2 4
   
y <- matrix( y , ncol=2 , byrow=TRUE)    
# define probabilities
set.seed(976)
pr <- stats::runif(10)
pr <- pr / sum(pr)
# sample data
N <- 300
x <- y[ sample( 1:10 , size=300, prob = pr , replace = TRUE ) , ]
w <- stats::runif( N , 0.5 , 1.5 )

# frequency table unweighted data
weighted_table(x[,2] )
table( x[,2] )

# weighted data and proportions
weighted_table(x[,2] , w=w, props=TRUE)

#*** contingency table
table( x[,1] , x[,2] )
weighted_table( x )
# table using weights
weighted_table( x , w=w )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
