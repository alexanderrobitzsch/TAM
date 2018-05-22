%% File Name: data.fims.Aus.Jpn.Rd
%% File Version: 2.12

\name{data.fims.Aus.Jpn.scored}
\alias{data.fims.Aus.Jpn.raw}
\alias{data.fims.Aus.Jpn.scored}
\docType{data}
\title{
Dataset FIMS Study with Responses of Australian and Japanese Students
}
\description{
Dataset FIMS study with raw responses (\code{data.fims.Aus.Jpn.raw}) or
scored responses (\code{data.fims.Aus.Jpn.scored}) of Australian and
Japanese Students.
}

\usage{
data(data.fims.Aus.Jpn.raw)
data(data.fims.Aus.Jpn.scored)
}
\format{
  A data frame with 6371 observations on the following 16 variables.
  \describe{
    \item{\code{SEX}}{Gender: 1 -- female, 2 -- male}
    \item{\code{M1PTI1}}{A Mathematics item}
    \item{\code{M1PTI2}}{A Mathematics item}
    \item{\code{M1PTI3}}{A Mathematics item}
    \item{\code{M1PTI6}}{A Mathematics item}
    \item{\code{M1PTI7}}{A Mathematics item}
    \item{\code{M1PTI11}}{A Mathematics item}
    \item{\code{M1PTI12}}{A Mathematics item}
    \item{\code{M1PTI14}}{A Mathematics item}
    \item{\code{M1PTI17}}{A Mathematics item}
    \item{\code{M1PTI18}}{A Mathematics item}
    \item{\code{M1PTI19}}{A Mathematics item}
    \item{\code{M1PTI21}}{A Mathematics item}
    \item{\code{M1PTI22}}{A Mathematics item}
    \item{\code{M1PTI23}}{A Mathematics item}
    \item{\code{country}}{Country: 1 -- Australia, 2 -- Japan}
  }
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

\seealso{
\url{http://www.edmeasurementsurveys.com/TAM/Tutorials/7DIF.htm}
}


\examples{
\dontrun{
data(data.fims.Aus.Jpn.scored)
#*****
# Model 1: Differential Item Functioning Gender for Australian students

# extract Australian students
scored <- data.fims.Aus.Jpn.scored[ data.fims.Aus.Jpn.scored$country==1, ]

# select items
items <- grep("M1", colnames(data.fims.Aus.Jpn.scored), value=TRUE)
##   > items
##    [1] "M1PTI1"  "M1PTI2"  "M1PTI3"  "M1PTI6"  "M1PTI7"  "M1PTI11" "M1PTI12"
##    [8] "M1PTI14" "M1PTI17" "M1PTI18" "M1PTI19" "M1PTI21" "M1PTI22" "M1PTI23"

# Run partial credit model
mod1 <- TAM::tam.mml(scored[,items])

# extract values of the gender variable into a variable called "gender".
gender <- scored[,"SEX"]
# computes the test score for each student by calculating the row sum
# of each student's scored responses.
raw_score <- rowSums(scored[,items] )

# compute the mean test score for each gender group: 1=male, and 2=female
stats::aggregate(raw_score,by=list(gender),FUN=mean)
# The mean test score is 6.12 for group 1 (males) and 6.27 for group 2 (females).
# That is, the two groups performed similarly, with girls having a slightly
# higher mean test score. The step of computing raw test scores is not necessary
# for the IRT analyses. But it's always a good practice to explore the data
# a little before delving into more complex analyses.

# Facets analysis
# To conduct a DIF analysis, we set up the variable "gender" as a facet and
# re-run the IRT analysis.
formulaA <- ~item+gender+item*gender    # define facets analysis
facets <- as.data.frame(gender)         # data frame with student covariates
# facets model for studying differential item functioning
mod2 <- TAM::tam.mml.mfr( resp=scored[,items], facets=facets, formulaA=formulaA )
summary(mod2)
}
}


\keyword{datasets}
