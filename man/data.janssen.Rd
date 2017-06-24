\name{data.janssen}
\alias{data.janssen}
\alias{data.janssen2}

\docType{data}
\title{
Dataset from Janssen and Geiser (2010)
}

\description{
Dataset used in Janssen and Geiser (2010).
}

\usage{
data(data.janssen)
data(data.janssen2)
}

\format{
\itemize{	

\item \code{data.janssen} is a data frame with 346 observations on the 8 
items of the following format

\code{'data.frame':   346 obs. of  8 variables:} \cr
\code{ $ PIS1 : num  1 1 1 0 0 1 1 1 0 1 ...} \cr
\code{ $ PIS3 : num  0 1 1 1 1 1 0 1 1 1 ...} \cr
\code{ $ PIS4 : num  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ PIS5 : num  0 1 1 0 1 1 1 1 1 0 ...} \cr
\code{ $ SCR6 : num  1 1 1 1 1 1 1 1 1 0 ...} \cr
\code{ $ SCR9 : num  1 1 1 1 0 0 0 1 0 0 ...} \cr
\code{ $ SCR10: num  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ SCR17: num  0 0 0 0 0 1 0 0 0 0 ...} \cr

\item \code{data.janssen2} contains 20 IST items:

\code{'data.frame':   346 obs. of  20 variables:} \cr
\code{ $ IST01 : num  1 1 1 0 0 1 1 1 0 1 ...} \cr
\code{ $ IST02 : num  1 0 1 0 1 1 1 1 0 1 ...} \cr
\code{ $ IST03 : num  0 1 1 1 1 1 0 1 1 1 ...} \cr
\code{[...]} \cr
\code{ $ IST020: num  0 0 0 1 1 0 0 0 0 0 ...} \cr
}
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}


\references{
Janssen, A. B., & Geiser, C. (2010). On the relationship between solution 
strategies in two mental rotation tasks. 
\emph{Learning and Individual Differences}, \bold{20(5)}, 473-478.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: CCT data, Janssen and Geiser (2010, LID)
#            Latent class analysis based on data.janssen
#############################################################################

data(data.janssen)
dat <- data.janssen
colnames(dat)
  ##   [1] "PIS1"  "PIS3"  "PIS4"  "PIS5"  "SCR6"  "SCR9"  "SCR10" "SCR17"

#*********************************************************************
#*** Model 1: Latent class analysis with two classes  

tammodel <- "
ANALYSIS:
  TYPE=LCA;
  NCLASSES(2);   
  NSTARTS(10,20); 
LAVAAN MODEL:
  # missing item numbers (e.g. PIS2) are ignored in the model
  F =~ PIS1__PIS5 + SCR6__SCR17
    "    
mod3 <- TAM::tamaan( tammodel , resp=dat  )
summary(mod3)

# extract item response functions
imod2 <- IRT.irfprob(mod3)[,2,]
# plot class specific probabilities
ncl <- 2
matplot( imod2 , type="o" , pch=1:ncl , xlab="Item" , ylab="Probability" )
legend( 1 , .3 , paste0("Class",1:ncl) , lty=1:ncl , col=1:ncl , pch=1:ncl )

#*********************************************************************
#*** Model 2: Latent class analysis with three classes  

tammodel <- "
ANALYSIS:
  TYPE=LCA;
  NCLASSES(3);   
  NSTARTS(10,20); 
LAVAAN MODEL:
  F =~ PIS1__PIS5 + SCR6__SCR17
    "    
mod3 <- TAM::tamaan( tammodel , resp=dat  )
summary(mod3)

# extract item response functions
imod2 <- IRT.irfprob(mod3)[,2,]
# plot class specific probabilities
ncl <- 3
matplot( imod2 , type="o" , pch=1:ncl , xlab="Item" , ylab="Probability" )
legend( 1 , .3 , paste0("Class",1:ncl) , lty=1:ncl , col=1:ncl , pch=1:ncl )

# compare models
AIC(mod1); AIC(mod2)		
}	
}
\keyword{datasets}
