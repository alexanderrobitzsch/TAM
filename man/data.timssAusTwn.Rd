%% File Name: data.timssAusTwn.Rd
%% File Version: 2.11

\name{data.timssAusTwn}
\alias{data.timssAusTwn}
\alias{data.timssAusTwn.scored}

\docType{data}
\title{
Dataset TIMSS 2011 of Australian and Taiwanese Students
}
\description{
Mathematics items of TIMSS 2011 of 1773 Australian and
Taiwanese students. The dataset \code{data.timssAusTwn} contains raw
responses while \code{data.timssAusTwn.scored} contains scored item
responses.
}
\usage{
data(data.timssAusTwn)
data(data.timssAusTwn.scored)
}
\format{
  A data frame with 1773 observations on the following 14 variables.
  \describe{
    \item{\code{M032166}}{a mathematics item}
    \item{\code{M032721}}{a mathematics item}
    \item{\code{M032757}}{a mathematics item}
    \item{\code{M032760A}}{a mathematics item}
    \item{\code{M032760B}}{a mathematics item}
    \item{\code{M032760C}}{a mathematics item}
    \item{\code{M032761}}{a mathematics item}
    \item{\code{M032692}}{a mathematics item}
    \item{\code{M032626}}{a mathematics item}
    \item{\code{M032595}}{a mathematics item}
    \item{\code{M032673}}{a mathematics item}
    \item{\code{IDCNTRY}}{Country identifier}
    \item{\code{ITSEX}}{Gender}
    \item{\code{IDBOOK}}{Booklet identifier}
  }
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

\seealso{
\url{http://www.edmeasurementsurveys.com/TAM/Tutorials/5PartialCredit.htm}

\url{http://www.edmeasurementsurveys.com/TAM/Tutorials/6Population.htm}
}

\examples{
data(data.timssAusTwn)
raw_resp <- data.timssAusTwn

#Recode data
resp <- raw_resp[,1:11]
      #Column 12 is country code. Column 13 is gender code. Column 14 is Book ID.
all.na <- rowMeans( is.na(resp) ) == 1
        #Find records where all responses are missing.
resp <- resp[!all.na,]              #Delete records with all missing responses
resp[resp==20 | resp==21] <- 2      #TIMSS double-digit coding: "20" or "21" is a score of 2
resp[resp==10 | resp==11] <- 1      #TIMSS double-digit coding: "10" or "11" is a score of 1
resp[resp==70 | resp==79] <- 0      #TIMSS double-digit coding: "70" or "79" is a score of 0
resp[resp==99] <- 0                 #"99" is omitted responses. Score it as wrong here.
resp[resp==96 | resp==6] <- NA      #"96" and "6" are not-reached items. Treat these as missing.

#Score multiple-choice items        #"resp" contains raw responses for MC items.
Scored <- resp
Scored[,9] <- (resp[,9]==4)*1       #Key for item 9 is D.
Scored[,c(1,2)] <- (resp[,c(1,2)]==2)*1  #Key for items 1 and 2 is B.
Scored[,c(10,11)] <- (resp[,c(10,11)]==3)*1  #Key for items 10 and 11 is C.

#Run IRT analysis for partial credit model (MML estimation)
mod1 <- TAM::tam.mml(Scored)

#Item parameters
mod1$xsi

#Thurstonian thresholds
tthresh <- TAM::tam.threshold(mod1)
tthresh

\dontrun{
#Plot Thurstonian thresholds
windows (width=8, height=7)
par(ps=9)
dotchart(t(tthresh), pch=19)
# plot expected response curves
plot( mod1 , ask=TRUE)

#Re-run IRT analysis in JML
mod1.2 <- TAM::tam.jml(Scored)
stats::var(mod1.2$WLE)

#Re-run the model with "not-reached" coded as incorrect.
Scored2 <- Scored
Scored2[is.na(Scored2)] <- 0

#Prepare anchor parameter values
nparam <- length(mod1$xsi$xsi)
xsi <- mod1$xsi$xsi
anchor <- matrix(c(seq(1,nparam),xsi), ncol=2)

#Run IRT with item parameters anchored on mod1 values
mod2 <- TAM::tam.mml(Scored2, xsi.fixed=anchor)

#WLE ability estimates
ability <- TAM::tam.wle(mod2)
ability

#CTT statistics
ctt <- TAM::tam.ctt(resp, ability$theta)
write.csv(ctt,"TIMSS_CTT.csv")

#plot histograms of ability and item parameters in the same graph
windows(width=4.45, height=4.45, pointsize=12)
layout(matrix(c(1,1,2),3,byrow=TRUE))
layout.show(2)
hist(ability$theta,xlim=c(-3,3),breaks=20)
hist(tthresh,xlim=c(-3,3),breaks=20)

#Extension
#Score equivalence table
dummy <- matrix(0,nrow=16,ncol=11)
dummy[lower.tri(dummy)] <- 1
dummy[12:16,c(3,4,7,8)][lower.tri(dummy[12:16,c(3,4,7,8)])]<-2

mod3 <- TAM::tam.mml(dummy, xsi.fixed=anchor)
wle3 <- TAM::tam.wle(mod3)
}
}


\keyword{datasets}
