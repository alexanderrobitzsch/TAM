%% File Name: plot.tam.Rd
%% File Version: 3.28

\name{plot.tam}
\alias{plot.tam}
\alias{plot.tam.mml}
\alias{plot.tam.jml}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Function for Unidimensional Item Response Models
}
\description{
S3 plot method for objects of class \code{tam}, \code{tam.mml}
or \code{tam.mml}.
}

\usage{
\method{plot}{tam}(x, items=1:x$nitems, type="expected", low=-3, high=3, ngroups=6, 
                   wle=NULL, export=TRUE, export.type="png", 
                   export.args=list(), observed=TRUE, overlay=FALSE , 
                   ask=FALSE, package="lattice" , fix.devices=FALSE , ...)
	
\method{plot}{tam.mml}(x, items=1:x$nitems, type="expected", low=-3, high=3, ngroups=6, 
                       wle=NULL, export=TRUE, export.type="png", 
                       export.args=list(), observed=TRUE, overlay=FALSE , 
                       ask=FALSE,  package="lattice" ,  fix.devices=FALSE , ...)
     
\method{plot}{tam.jml}(x, items=1:x$nitems, type="expected", low=-3, high=3, ngroups=6, 
                       wle=NULL, export=TRUE, export.type="png", 
                       export.args=list(), observed=TRUE, overlay=FALSE , 
                       ask=FALSE,  package="lattice" , fix.devices=FALSE , ...)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{
Object of class \code{tam}, \code{tam.mml}
or \code{tam.mml}.
}
\item{items}{
  An index vector giving the items to be visualized.
}
\item{type}{Plot type. \code{type="expected"} plot the expected item
	response curves while \code{type="items"} plots the response
	curves of all item categories.}
  \item{low}{
Lowest \eqn{\theta} value to be displayed
}
  \item{high}{
Highest \eqn{\theta} value to be displayed
}
  \item{ngroups}{
Number of score groups to be displayed. The default are
six groups.
}
  \item{wle}{
Use WLE estimate for displaying observed scores.
}
  \item{export}{
A logical which indicates whether all graphics should be separately 
exported in files of type \code{export.type} in a subfolder \code{'Plots'}
of the working directory.
}
\item{export.type}{
  A string which indicates the type of the graphics export. For currently 
  supported file types, see     
\code{\link[grDevices:dev.new]{grDevices::dev.new}}.

}
\item{export.args}{
  A list of arguments that are passed to the export method can be specified. 
  See the respective export device method for supported usage.  
}
\item{observed}{
  A logical which indicates whether observed response curve should
  be displayed
}
\item{overlay}{
  A logical indicating whether expected score functions should overlay.
}
\item{ask}{
  A logical which asks for changing the graphic from item to item.
  The default is \code{FALSE}.
}
\item{package}{Used \R package for plot. Can be \code{"lattice"} or
\code{"graphics"}.}
\item{fix.devices}{Optional logical indicating whether old graphics devices should
	 be saved.}
\item{\dots}{
  Further arguments to be passed
}
}

\details{
This plot method does not work for multidimensional item response
models.
}

%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Margaret Wu, Thomas Kiefer, Alexander Robitzsch, Michal Modzelewski
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link[CDM:IRT.irfprobPlot]{CDM::IRT.irfprobPlot}}
for a general plot method.
}

\examples{
\dontrun{	
#############################################################################
# EXAMPLE 1: Dichotomous data data.sim.rasch
#############################################################################

data(data.sim.rasch)
mod <- TAM::tam.mml(data.sim.rasch)
# expected response curves
plot(mod, items=1:5, export=FALSE)
# item response curves
plot(mod, items=1:5, type="items" , export=FALSE)
# plot with graphics package
plot(mod, items=1:5, type="items" , export=FALSE , ask=TRUE , package="graphics")

#############################################################################
# EXAMPLE 2: Polytomous data
#############################################################################

data(data.Students, package="CDM")
dat <- data.Students[ , c("sc3","sc4" , "mj1" , "mj2" )]
dat <- na.omit(dat)
dat[ dat[,1] == 3 , 1 ] <- 2   # modify data
dat[ 1:20, 2 ] <- 4

# estimate model
mod1 <- TAM::tam.mml( dat )
# plot item response curves and expected response curves
plot(mod1, type="items", export=FALSE)
plot(mod1, type="expected", export=FALSE )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
