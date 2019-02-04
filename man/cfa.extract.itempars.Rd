%% File Name: cfa.extract.itempars.Rd
%% File Version: 0.22

\name{cfa.extract.itempars}
\alias{cfa.extract.itempars}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Extracting Item Parameters from a Fitted \code{cfa} Object in \pkg{lavaan}
}

\description{
This function extract item parameters from a fitted
\code{\link[lavaan:cfa]{lavaan::cfa}} object in \pkg{lavaan}. It
extract item loadings, item intercepts and the mean
and covariance matrix of latent variables in a
confirmatory factor analysis model.
}

\usage{
cfa.extract.itempars(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Fitted \code{cfa} object
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List with following entries
\item{L}{Matrix of item loadings}
\item{nu}{Vector of item intercepts}
\item{psi}{Residual covariance matrix}
\item{Sigma}{Covariance matrix of latent variables}
\item{nu}{Vector of means of latent variables}
\item{\dots}{Further values}
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
See \code{\link{IRTLikelihood.cfa}} for extracting the
individual likelihood from fitted confirmatory
factor analyses.

\code{\link[lavaan:cfa]{lavaan::cfa}}
}



\examples{
#############################################################################
# EXAMPLE 1: CFA data.Students
#############################################################################

library(lavaan)
library(CDM)

data(data.Students, package="CDM")
dat <- data.Students

dat1 <- dat[, paste0( "mj", 1:4 ) ]

#*** Model 1: Unidimensional model scale mj
lavmodel <- "
   mj=~ mj1 + mj2 + mj3 + mj4
   mj ~~ mj
     "
mod1 <- lavaan::cfa( lavmodel, data=dat1, std.lv=TRUE )
summary(mod1, standardized=TRUE, rsquare=TRUE )
# extract parameters
res1 <- TAM::cfa.extract.itempars( mod1 )

\dontrun{
#*** Model 2: Scale mj - explicit modelling of item intercepts
lavmodel <- "
   mj=~ mj1 + mj2 + mj3 + mj4
   mj ~~ mj
   mj1 ~ 1
     "
mod2 <- lavaan::cfa( lavmodel, data=dat1, std.lv=TRUE )
summary(mod2, standardized=TRUE, rsquare=TRUE )
res2 <- TAM::cfa.extract.itempars( mod2 )

#*** Model 3: Tau-parallel measurements scale mj
lavmodel <- "
   mj=~ a*mj1 + a*mj2 + a*mj3 + a*mj4
   mj ~~ 1*mj
   mj1 ~ b*1
   mj2 ~ b*1
   mj3 ~ b*1
   mj4 ~ b*1
     "
mod3 <- lavaan::cfa( lavmodel, data=dat1, std.lv=TRUE )
summary(mod3, standardized=TRUE, rsquare=TRUE )
res3 <- TAM::cfa.extract.itempars( mod3 )

#*** Model 4: Two-dimensional CFA with scales mj and sc
dat2 <- dat[, c(paste0("mj",1:4), paste0("sc",1:4)) ]
# lavaan model with shortage "__" operator
lavmodel <- "
   mj=~ mj1__mj4
   sc=~ sc1__sc4
   mj ~~ sc
   mj ~~ 1*mj
   sc ~~ 1*sc
     "
lavmodel <- TAM::lavaanify.IRT( lavmodel, data=dat2 )$lavaan.syntax
cat(lavmodel)
mod4 <- lavaan::cfa( lavmodel, data=dat2, std.lv=TRUE )
summary(mod4, standardized=TRUE, rsquare=TRUE )
res4 <- TAM::cfa.extract.itempars( mod4 )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Confirmatory factor analysis}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
