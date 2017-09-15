%% File Name: IRT.truescore.Rd
%% File Version: 0.15
%% File Last Change: 2017-06-22 17:41:51

\name{IRT.truescore}
\alias{IRT.truescore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts a \eqn{\theta} Score into a True Score \eqn{\tau ( \theta)}
}
\description{
Converts a \eqn{\theta} score into an unweighted true score 
\eqn{\tau ( \theta) = \sum_i \sum_h h P_i ( \theta ) }.
In addition, a weighted true score
\eqn{\tau ( \theta) = \sum_i \sum_h q_{ih} P_i ( \theta ) }
can also be computed by specifying item-category weights
\eqn{q_{ih}} in the matrix \code{Q}.
}
\usage{
IRT.truescore(object, iIndex = NULL, theta = NULL, Q = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object for which the 
\code{\link[CDM:IRT.irfprob]{CDM::IRT.irfprob}} S3 method is defined 
}
  \item{iIndex}{
Optional vector with item indices
}
  \item{theta}{
Optional vector with \eqn{\theta} values
}
\item{Q}{Optional weighting matrix}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Data frame containing \eqn{\theta} values and corresponding
true scores \eqn{\tau( \theta ) }.
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
See also \code{\link[sirt:truescore.irt]{sirt::truescore.irt}} 
for a conversion function for generalized partial credit models.
}

\examples{
#############################################################################
# EXAMPLE 1: True score conversion for a test with polytomous items
#############################################################################

data(data.Students, package="CDM")
dat <- data.Students[ , paste0("mj",1:4) ]

# fit partial credit model
mod1 <- TAM::tam.mml( dat ,control=list(maxiter=20) )
summary(mod1)

# true score conversion
tmod1 <- TAM::IRT.truescore( mod1 )
round( tmod1 , 4 )
# true score conversion with user-defined theta grid
tmod1b <- TAM::IRT.truescore( mod1 , theta=seq( -8,8, len=33 ) )
# plot results
plot( tmod1$theta , tmod1$truescore , type="l" , 
            xlab=expression(theta) , ylab=expression(tau( theta ) ) )
points( tmod1b$theta , tmod1b$truescore , pch=16 , col="brown" )

\dontrun{
#############################################################################
# EXAMPLE 2: True scores with different category weightings
#############################################################################

data(data.timssAusTwn.scored)
dat <- data.timssAusTwn.scored
# extract item response data
dat <- dat[ , grep("M03" , colnames(dat) ) ]
# select items with do have maximum score of 2 (polytomous items)
ind <- which( apply( dat ,  2, max , na.rm=TRUE ) == 2 )
I <- ncol(dat)
# define Q-matrix with scoring variant
Q <- matrix( 1 , nrow=I , ncol=1 )
Q[ ind , 1 ] <- .5    # score of 0.5 for polyomous items 

# estimate model
mod1 <- TAM::tam.mml( dat , Q=Q , irtmodel="PCM2" , control=list( nodes=seq(-10,10,len=61) ) )
summary(mod1)

# true score with scoring (0,1,2) which is the default of the function
tmod1 <- TAM::IRT.truescore(mod1)
# true score with user specified weighting matrix
Q <- mod1$B[,,1]
tmod2 <- TAM::IRT.truescore(mod1, Q=Q)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{True scores}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
