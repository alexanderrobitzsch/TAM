%% File Name: data.numeracy.Rd
%% File Version: 2.08
%% File Last Change: 2017-06-22 17:41:51

\name{data.numeracy}
\alias{data.numeracy}
\docType{data}

\title{
Dataset Numeracy
}


\description{
Dataset numeracy with unscored (\code{raw}) and scored (\code{scored}) 
item responses of 876 persons and 15 items.
}


\usage{
data(data.numeracy)
}

\format{
The format is a list a two entries: 

\code{List of 2} \cr
\code{ $ raw   :'data.frame':} \cr
\code{  ..$ I1 : int [1:876] 1 0 1 0 0 0 0 0 1 1 ...} \cr
\code{  ..$ I2 : int [1:876] 0 1 0 0 1 1 1 1 1 0 ...} \cr
\code{  ..$ I3 : int [1:876] 4 4 1 3 4 4 4 4 4 4 ...} \cr
\code{  ..$ I4 : int [1:876] 4 1 2 2 1 1 1 1 1 1 ...} \cr
\code{  [...] } \cr
\code{  ..$ I15: int [1:876] 1 1 1 1 0 1 1 1 1 1 ...} \cr
\code{ $ scored:'data.frame':} \cr
\code{  ..$ I1 : int [1:876] 1 0 1 0 0 0 0 0 1 1 ...} \cr
\code{  ..$ I2 : int [1:876] 0 1 0 0 1 1 1 1 1 0 ...} \cr
\code{  ..$ I3 : int [1:876] 1 1 0 0 1 1 1 1 1 1 ...} \cr
\code{  ..$ I4 : int [1:876] 0 1 0 0 1 1 1 1 1 1 ...} \cr
\code{  [...] } \cr
\code{  ..$ I15: int [1:876] 1 1 1 1 0 1 1 1 1 1 ...} \cr
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
%%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%%}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
######################################################################
# (1) Scored numeracy data
######################################################################

data(data.numeracy)
dat <- data.numeracy$scored

#Run IRT analysis: Rasch model
mod1 <- TAM::tam.mml(dat)

#Item difficulties
mod1$xsi
ItemDiff <- mod1$xsi$xsi
ItemDiff

#Ability estimate - Weighted Likelihood Estimate
Abil <- TAM::tam.wle(mod1)
Abil
PersonAbility <- Abil$theta
PersonAbility

#Descriptive statistics of item and person parameters
hist(ItemDiff)
hist(PersonAbility)
mean(ItemDiff)
mean(PersonAbility)
stats::sd(ItemDiff)
stats::sd(PersonAbility)

\dontrun{
#Extension
#plot histograms of ability and item parameters in the same graph
oldpar <- par(no.readonly = TRUE)	      # save writable default graphic settings
windows(width=4.45, height=4.45, pointsize=12)
layout(matrix(c(1,1,2),3,byrow=TRUE))
layout.show(2)
hist(PersonAbility,xlim=c(-3,3),breaks=20)
hist(ItemDiff,xlim=c(-3,3),breaks=20)

par( oldpar )  # restore default graphic settings
hist(PersonAbility,xlim=c(-3,3),breaks=20)

######################################################################
# (2) Raw numeracy data
######################################################################

raw_resp <- data.numeracy$raw

#score responses
key <- c(1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1)
scored <- sapply( seq(1,length(key)) , 
            FUN = function(ii){ 1*(raw_resp[,ii] == key[ii]) } )

#run IRT analysis
mod1 <- TAM::tam.mml(scored)

#Ability estimate - Weighted Likelihood Estimate
Abil <- TAM::tam.wle(mod1)

#CTT statistics
ctt1 <- TAM::tam.ctt(raw_resp, Abil$theta)
write.csv(ctt1,"D1_ctt1.csv")       # write statistics into a file
        # use maybe write.csv2 if ';' should be the column separator

#Fit statistics
Fit <- TAM::tam.fit(mod1)
Fit

# plot expected response curves
plot( mod1 , ask=TRUE ) 
}
}



\keyword{datasets}
