%% File Name: data.cqc.Rd
%% File Version: 2.28

\name{data.cqc}


\alias{data.cqc01}
\alias{data.cqc02}
\alias{data.cqc03}
\alias{data.cqc04}
\alias{data.cqc05}

\docType{data}
\title{
More Datasets and Examples (Similar to ConQuest Examples)
}


\description{
Datasets and examples similar to the ones in the ConQuest manual
(Wu, Adams, Wilson, & Haldane, 2007).
}


\usage{
data(data.cqc01)
data(data.cqc02)
data(data.cqc03)
data(data.cqc04)
data(data.cqc05)
}


\format{

\itemize{
\item \code{data.cqc01} contains 512 persons on
12 dichotomous items of following format

\code{'data.frame':   512 obs. of  12 variables:} \cr
\code{ $ BSMMA01: int  1 1 0 1 1 1 1 1 0 0 ...} \cr
\code{ $ BSMMA02: int  1 0 1 1 0 1 1 1 0 0 ...} \cr
\code{ $ BSMMA03: int  1 1 0 1 1 1 1 1 1 0 ...} \cr
\code{ [...]} \cr
\code{ $ BSMSA12: int  0 0 0 0 1 0 1 1 0 0 ...} \cr

\item \code{data.cqc02} contains 431 persons on 8 polytomous
variables of following format

\code{'data.frame':   431 obs. of  8 variables:} \cr
\code{ $ It1: int  1 1 2 0 2 1 2 2 2 1 ...} \cr
\code{ $ It2: int  3 0 1 2 2 3 2 2 1 1 ...} \cr
\code{ $ It3: int  1 1 1 0 1 1 0 0 1 0 ...} \cr
\code{ [...]} \cr
\code{ $ It8: int  3 1 0 0 3 1 3 0 3 0 ...} \cr

\item \code{data.cqc03} contains 11200 observations for
5600 persons, 16 raters and 2 items (\code{crit1} and \code{crit2})

\code{'data.frame':   11200 obs. of  4 variables:} \cr
\code{ $ pid  : num  10001 10001 10002 10002 10003 ...} \cr
\code{ $ rater: chr  "R11" "R12" "R13" "R14" ...} \cr
\code{ $ crit1: int  2 2 2 1 3 2 2 1 1 1 ...} \cr
\code{ $ crit2: int  3 3 2 1 2 2 2 2 2 1 ...} \cr

\item \code{data.cqc04} contains 1452 observations for 363 persons,
4 raters, 4 topics and 5 items (\code{spe}, \code{coh}, \code{str},
 \code{gra},  \code{con})

\code{'data.frame':   1452 obs. of  8 variables:} \cr
\code{ $ pid  : num  10010 10010 10010 10010 10016 ...} \cr
\code{ $ rater: chr  "BE" "CO" "BE" "CO" ...} \cr
\code{ $ topic: chr  "Spor" "Spor" "Spor" "Spor" ...} \cr
\code{ $ spe  : int  2 0 2 1 3 3 3 3 3 2 ...} \cr
\code{ $ coh  : int  1 1 2 0 3 3 3 3 3 3 ...} \cr
\code{ $ str  : int  0 1 3 0 3 2 3 2 3 0 ...} \cr
\code{ $ gra  : int  0 0 2 0 3 3 3 3 2 1 ...} \cr
\code{ $ con  : int  0 0 0 0 3 1 2 2 3 0 ...} \cr

\item \code{data.cqc05} contains 1500 persons,
3 covariates and 157 items.

\code{'data.frame':   1500 obs. of  160 variables:} \cr
\code{ $ gender: int  1 0 1 0 0 0 0 1 0 1 ...} \cr
\code{ $ level : int  0 1 1 0 0 0 1 0 1 1 ...} \cr
\code{ $ gbyl  : int  0 0 1 0 0 0 0 0 0 1 ...} \cr
\code{ $ A001  : num  0 0 0 1 0 1 1 1 0 1 ...} \cr
\code{ $ A002  : num  1 1 0 1 1 1 1 1 1 0 ...} \cr
\code{ $ A003  : num  0 0 0 0 1 1 1 0 0 1 ...} \cr
\code{[...]} \cr


}
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}


\seealso{
See the \code{\link[sirt:R2conquest]{sirt::R2conquest}} function
for running ConQuest software from within \R.

See the \pkg{\link[WrightMap:WrightMap]{WrightMap}} package for functions
connected to reading ConQuest files and creating Wright maps.
ConQuest output files can be read into \R with the help of
the \code{\link[WrightMap:CQmodel]{WrightMap::CQmodel}} function.
See also the \code{\link{IRT.WrightMap}} function in \pkg{TAM}.

See also the \pkg{eat} package (\url{https://r-forge.r-project.org/projects/eat/})
for elaborate functionality for communication of ConQuest with \R.
}
% https://r-forge.r-project.org/projects/eat/

%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

\references{
Wu, M. L., Adams, R. J., Wilson, M. R. & Haldane, S. (2007).
\emph{ACER ConQuest Version 2.0}. Mulgrave.
https://shop.acer.edu.au/acer-shop/group/CON3.
}


\examples{
\dontrun{

library(sirt)
library(WrightMap)
# In the following, ConQuest will also be used for estimation.
path.conquest <- "C:/Conquest"             # path of the ConQuest console.exe
setwd( "p:/my_files/ConQuest_analyses" )  # working directory

#############################################################################
# EXAMPLE 01: Rasch model data.cqc01
#############################################################################

data(data.cqc01)
dat <- data.cqc01

#********************************************
#*** Model 01: Estimate Rasch model
mod01 <- TAM::tam.mml(dat)
summary(mod01)

#------- ConQuest

# estimate model
cmod01 <- sirt::R2conquest( dat, name="mod01", path.conquest=path.conquest)
summary(cmod01)   # summary output
# read shw file with some terms
shw01a <- sirt::read.show( "mod01.shw" )
cmod01$shw.itemparameter
# read person item maps
pi01a <- sirt::read.pimap( "mod01.shw" )
cmod01$shw.pimap
# read plausible values (npv=10 plausible values)
pv01a <- sirt::read.pv(pvfile="mod01.pv", npv=10)
cmod01$person

# read ConQuest model
res01a <- WrightMap::CQmodel(p.est="mod01.wle", show="mod01.shw", p.type="WLE" )
print(res01a)
# plot item fit
WrightMap::fitgraph(res01a)
# Wright map
plot(res01a, label.items.srt=90 )

#############################################################################
# EXAMPLE 02: Partial credit model and rating scale model data.cqc02
#############################################################################

data(data.cqc02)
dat <- data.cqc02

#********************************************
# Model 02a: Partial credit model in ConQuest parametrization 'item+item*step'
mod02a <- TAM::tam.mml( dat, irtmodel="PCM2" )
summary(mod02a, file="mod02a")
fit02a <- TAM::tam.fit(mod02a)
summary(fit02a)

#--- ConQuest
# estimate model
maxK <- max( dat, na.rm=TRUE )
cmod02a <- sirt::R2conquest( dat, itemcodes=0:maxK, model="item+item*step",
               name="mod02a", path.conquest=path.conquest)
summary(cmod02a)   # summary output

# read ConQuest model
res02a <- WrightMap::CQmodel(p.est="mod02a.wle", show="mod02a.shw", p.type="WLE" )
print(res02a)
# Wright map
plot(res02a, label.items.srt=90 )
plot(res02a, item.table="item")

#********************************************
# Model 02b: Rating scale model
mod02b <- TAM::tam.mml( dat, irtmodel="RSM" )
summary( mod02b )

#############################################################################
# EXAMPLE 03: Faceted Rasch model for rating data data.cqc03
#############################################################################

data(data.cqc03)
# select items
resp <- data.cqc03[, c("crit1","crit2") ]

#********************************************
# Model 03a: 'item+step+rater'
mod03a <- TAM::tam.mml.mfr( resp, facets=data.cqc03[,"rater",drop=FALSE],
            formulaA=~ item+step+rater, pid=data.cqc03$pid )
summary( mod03a )

#--- ConQuest
X <- data.cqc03[,"rater",drop=FALSE]
X$rater <- as.numeric(substring( X$rater, 2 )) # convert 'rater' in numeric format
maxK <- max( resp, na.rm=TRUE)
cmod03a <- sirt::R2conquest( resp,  X=X, regression="",  model="item+step+rater",
             name="mod03a", path.conquest=path.conquest, set.constraints="cases" )
summary(cmod03a)   # summary output

# read ConQuest model
res03a <- WrightMap::CQmodel(p.est="mod03a.wle", show="mod03a.shw", p.type="WLE" )
print(res03a)
# Wright map
plot(res03a)

#********************************************
# Model 03b: 'item:step+rater'
mod03b <- TAM::tam.mml.mfr( resp, facets=data.cqc03[,"rater",drop=FALSE],
            formulaA=~ item + item:step+rater, pid=data.cqc03$pid )
summary( mod03b )

#********************************************
# Model 03c: 'step+rater' for first item 'crit1'
# Restructuring the data is necessary.
# Define raters as items in the new dataset 'dat1'.
persons <- unique( data.cqc03$pid )
raters <- unique( data.cqc03$rater )
dat1 <- matrix( NA, nrow=length(persons), ncol=length(raters) + 1 )
dat1 <- as.data.frame(dat1)
colnames(dat1) <- c("pid", raters )
dat1$pid <- persons
for (rr in raters){
    dat1.rr <- data.cqc03[ data.cqc03$rater==rr, ]
    dat1[ match(dat1.rr$pid, persons),rr] <- dat1.rr$crit1
        }
  ##   > head(dat1)
  ##       pid R11 R12 R13 R14 R15 R16 R17 R18 R19 R20 R21 R22 R23 R24 R25 R26
  ##   1 10001   2   2  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
  ##   2 10002  NA  NA   2   1  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
  ##   3 10003  NA  NA   3   2  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
  ##   4 10004  NA  NA   2   1  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
  ##   5 10005  NA  NA   1   1  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
  ##   6 10006  NA  NA   1   1  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
# estimate model 03c
mod03c <- TAM::tam.mml( dat1[,-1], pid=dat1$pid )
summary( mod03c )

#############################################################################
# EXAMPLE 04: Faceted Rasch model for rating data data.cqc04
#############################################################################

data(data.cqc04)
resp <- data.cqc04[,4:8]
facets <- data.cqc04[, c("rater", "topic") ]

#********************************************
# Model 04a: 'item*step+rater+topic'
formulaA <- ~ item*step + rater + topic
mod04a <- TAM::tam.mml.mfr( resp, facets=facets,
            formulaA=formulaA, pid=data.cqc04$pid )
summary( mod04a )

#********************************************
# Model 04b: 'item*step+rater+topic+item*rater+item*topic'
formulaA <- ~ item*step + rater + topic + item*rater + item*topic
mod04b <- TAM::tam.mml.mfr( resp, facets=facets,
            formulaA=formulaA, pid=data.cqc04$pid )
summary( mod04b )

#********************************************
# Model 04c: 'item*step' with fixing rater and topic parameters to zero
formulaA <- ~ item*step + rater + topic
mod04c0 <- TAM::tam.mml.mfr( resp, facets=facets,
            formulaA=formulaA, pid=data.cqc04$pid, control=list(maxiter=4) )
summary( mod04c0 )
# fix rater and topic parameter to zero
xsi.est <- mod04c0$xsi
xsi.fixed <- cbind( seq(1,nrow(xsi.est)), xsi.est$xsi )
rownames(xsi.fixed) <- rownames(xsi.est)
xsi.fixed <- xsi.fixed[ c(8:13),]
xsi.fixed[,2] <- 0
  ##   > xsi.fixed
  ##             [,1] [,2]
  ##   raterAM      8    0
  ##   raterBE      9    0
  ##   raterCO     10    0
  ##   topicFami   11    0
  ##   topicScho   12    0
  ##   topicSpor   13    0
mod04c1 <- TAM::tam.mml.mfr( resp, facets=facets,
             formulaA=formulaA, pid=data.cqc04$pid, xsi.fixed=xsi.fixed )
summary( mod04c1 )

#############################################################################
# EXAMPLE 05: Partial credit model with latent regression and
#             plausible value imputation
#############################################################################

data(data.cqc05)
resp <- data.cqc05[, -c(1:3) ] # select item responses

#********************************************
# Model 05a: Partial credit model
mod05a <-tam.mml(resp=resp, irtmodel="PCM2" )

#********************************************
# Model 05b: Partial credit model with latent regressors
mod05b <-tam.mml(resp=resp, irtmodel="PCM2",  Y=data.cqc05[,1:3] )
# Plausible value imputation
pvmod05b <- TAM::tam.pv( mod05b )
}
}

\keyword{datasets}
