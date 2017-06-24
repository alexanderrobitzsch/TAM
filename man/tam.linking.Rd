\name{tam.linking}
\alias{tam.linking}
\alias{summary.tam.linking}
\alias{tam_linking_2studies}
\alias{summary.tam_linking_2studies}
\alias{print.tam.linking}
\alias{print.tam_linking_2studies}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Linking of Fitted Unidimensional Item Response Models in \pkg{TAM}
}


\description{
Performs linking of fitted unidimensional item response models in \pkg{TAM}
according to the Stocking-Lord and the Haebara method (Kolen & Brennan, 2014;
Gonzales & Wiberg, 2017).
Several studies are linking by a chain of linkings of two studies. The linking 
of two studies is implemented in the \code{tam_linking_2studies} function. 
}

\usage{
tam.linking(tamobj_list, type = "SL", theta = NULL, wgt = NULL, fix.slope = FALSE, 
   verbose = TRUE)

\method{summary}{tam.linking}(object, file=NULL, \dots)  

\method{print}{tam.linking}(x, \dots) 

tam_linking_2studies( B1, AXsi1, guess1, B2, AXsi2, guess2, theta, wgt, type, 
    M1=0, SD1=1, M2=0, SD2=1, fix.slope=FALSE)
   
\method{summary}{tam_linking_2studies}(object, file=NULL, \dots)

\method{print}{tam_linking_2studies}(x, \dots)    
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tamobj_list}{
List of fitted objects in \pkg{TAM}
}
  \item{type}{
Type of linking method: \code{"SL"} (Stocking-Lord) or \code{"Hae"} (Haebara).
The default is the Stocking-Lord methode.
}
  \item{theta}{
Grid of \eqn{\theta} points. The default is \code{seq(-6,6,len=101)}.
}
  \item{wgt}{
Weights defined for the \code{theta} grid. The default is \cr
\code{tam_normalize_vector( stats::dnorm( theta , sd = 2 ))}.
}
  \item{fix.slope}{
Logical indicating whether the slope transformation constant is fixed to 1.
}
\item{verbose}{
Logical indicating progress of linking computation
}
\item{object}{Object of class \code{tam.linking} or \code{tam_linking_2studies}.}
\item{x}{Object of class \code{tam.linking} or \code{tam_linking_2studies}.}
\item{file}{A file name in which the summary output will be written}
\item{\dots}{Further arguments to be passed}
\item{B1}{Array \eqn{B} for first study}
\item{AXsi1}{Matrix \eqn{A \xi} for first study}
\item{guess1}{Guessing parameter for first study}
\item{B2}{Array \eqn{B} for second study}
\item{AXsi2}{Matrix \eqn{A \xi} for second study}
\item{guess2}{Guessing parameter for second study}
\item{M1}{Mean of first study}
\item{SD1}{Standard deviation of first study}
\item{M2}{Mean of second study}
\item{SD2}{Standard deviation of second study}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List containing entries

\item{parameters_list}{List containing transformed item parameters}
\item{linking_list}{List containing results of each linking in the
   linking chain}
 \item{M_SD}{Mean and standard deviation for each study after linking}
 \item{trafo_items}{Transformation constants for item parameters}
 \item{trafo_persons}{Transformation constants for person parameters} 
}


\references{
Battauz, M. (2015). \pkg{equateIRT}: An \R package for IRT test equating.
\emph{Journal of Statistical Software, 68}(7), 1-22.		

Gonzalez, J., & Wiberg, M. (2017). 
\emph{Applying test equating methods: Using \R}. New York, Springer.

Kolen, M. J., & Brennan, R. L. (2014). \emph{Test equating, scaling, and linking: 
Methods and practices}. New York, Springer.

Weeks, J. P. (2010). \pkg{plink}: An \R package for linking mixed-format tests 
using IRT-based methods. \emph{Journal of Statistical Software, 35}(12), 1-33.
}

%\author{
%%  ~~who you are~~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Linking of item response models can be also conducted with \pkg{plink} 
(Weeks, 2010) or \pkg{equateIRT} (Battauz, 2015) packages. See also the
\code{\link[sirt:linking.haberman]{sirt::linking.haberman}} in the \pkg{sirt}
package.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Linking dichotomous data with the 2PL model
#############################################################################

data(data.ex16)
dat <- data.ex16
items <- colnames(dat)[-c(1,2)]

# fit grade 1
rdat1 <- TAM::tam_remove_missings( dat[ dat$grade == 1 , ], items=items )
mod1 <- TAM::tam.mml.2pl( resp= rdat1$resp[, rdat1$items] , pid = rdat1$dat$idstud )
summary(mod1)

# fit grade 2
rdat2 <- TAM::tam_remove_missings( dat[ dat$grade == 2 , ], items=items )
mod2 <- TAM::tam.mml.2pl( resp= rdat2$resp[, rdat2$items] , pid = rdat2$dat$idstud )
summary(mod2)

# fit grade 3
rdat3 <- TAM::tam_remove_missings( dat[ dat$grade == 3 , ], items=items )
mod3 <- TAM::tam.mml.2pl( resp= rdat3$resp[, rdat3$items] , pid = rdat3$dat$idstud )
summary(mod3)

# define list of fitted models
tamobj_list <- list( mod1, mod2 , mod3 )

#-- link item response models
lmod <- TAM::tam.linking( tamobj_list)
summary(lmod)

# estimate WLEs based on transformed item parameters
parm_list <- lmod$parameters_list

# WLE grade 1
arglist <- list( resp = mod1$resp, B = parm_list[[1]]$B, AXsi = parm_list[[1]]$AXsi )
wle1 <- TAM::tam.mml.wle(tamobj=arglist)

# WLE grade 2
arglist <- list( resp = mod2$resp, B = parm_list[[2]]$B, AXsi = parm_list[[2]]$AXsi )
wle2 <- TAM::tam.mml.wle(tamobj=arglist)

# WLE grade 3
arglist <- list( resp = mod3$resp, B = parm_list[[3]]$B, AXsi = parm_list[[3]]$AXsi )
wle3 <- TAM::tam.mml.wle(tamobj=arglist)

#############################################################################
# EXAMPLE 2: Linking polytomous data with the partial credit model
#############################################################################

data(data.ex17)
dat <- data.ex17

items <- colnames(dat)[-c(1,2)]

# fit grade 1
rdat1 <- TAM::tam_remove_missings( dat[ dat$grade == 1 , ], items=items )
mod1 <- TAM::tam.mml.2pl( resp= rdat1$resp[, rdat1$items] , pid = rdat1$dat$idstud )
summary(mod1)

# fit grade 2
rdat2 <- TAM::tam_remove_missings( dat[ dat$grade == 2 , ], items=items )
mod2 <- TAM::tam.mml.2pl( resp= rdat2$resp[, rdat2$items] , pid = rdat2$dat$idstud )
summary(mod2)

# fit grade 3
rdat3 <- TAM::tam_remove_missings( dat[ dat$grade == 3 , ], items=items )
mod3 <- TAM::tam.mml.2pl( resp= rdat3$resp[, rdat3$items] , pid = rdat3$dat$idstud )
summary(mod3)

# list of fitted TAM models
tamobj_list <- list( mod1, mod2 , mod3 )

#-- linking: fix slope because partial credit model is fitted
lmod <- TAM::tam.linking( tamobj_list, fix.slope=TRUE)
summary(lmod)

# WLEs can be estimated in the same way as in Example 1.

#############################################################################
# EXAMPLE 3: Linking dichotomous data with the multiple group 2PL models
#############################################################################

data(data.ex16)
dat <- data.ex16
items <- colnames(dat)[-c(1,2)]

# fit grade 1
rdat1 <- TAM::tam_remove_missings( dat[ dat$grade == 1 , ], items=items )
# create some grouping variable
group <- ( seq( 1 , nrow( rdat1$dat ) ) \%\%\ 3 ) + 1
mod1 <- TAM::tam.mml.2pl( resp= rdat1$resp[, rdat1$items] , pid = rdat1$dat$idstud, group=group)
summary(mod1)

# fit grade 2
rdat2 <- TAM::tam_remove_missings( dat[ dat$grade == 2 , ], items=items )
group <- 1*(rdat2$dat$dat$idstud > 500)
mod2 <- TAM::tam.mml.2pl( resp=rdat2$resp[, rdat2$items], pid=rdat2$dat$dat$idstud, group=group)
summary(mod2)

# fit grade 3
rdat3 <- TAM::tam_remove_missings( dat[ dat$grade == 3, ], items=items )
mod3 <- TAM::tam.mml.2pl( resp= rdat3$resp[, rdat3$items], pid = rdat3$dat$idstud )
summary(mod3)

# define list of fitted models
tamobj_list <- list( mod1, mod2 , mod3 )

#-- link item response models
lmod <- TAM::tam.linking( tamobj_list)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
