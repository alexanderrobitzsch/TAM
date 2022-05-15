%% File Name: anova.tam.Rd
%% File Version: 2.341

\name{anova-logLik}
\alias{anova.tam}
\alias{anova.tam.mml}
\alias{anova.tam.mml.3pl}
\alias{anova.tamaan}
\alias{anova.tam.latreg}
\alias{anova.tam.np}

\alias{logLik.tam}
\alias{logLik.tam.mml}
\alias{logLik.tam.mml.3pl}
\alias{logLik.tamaan}
\alias{logLik.tam.latreg}
\alias{logLik.tam.np}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Likelihood Ratio Test for Model Comparisons and Log-Likelihood Value
}


\description{
The \code{anova} function compares two models estimated of class \code{\link{tam}},
\code{\link{tam.mml}} or \code{\link{tam.mml.3pl}} using a likelihood ratio test.
The \code{logLik} function extracts the value of the log-Likelihood.

The function can be applied for values of \code{\link{tam.mml}},
\code{\link{tam.mml.2pl}}, \code{\link{tam.mml.mfr}}, \code{\link{tam.fa}},
\code{\link{tam.mml.3pl}}, \code{\link{tam.latreg}} or \code{\link{tamaan}}.
}


\usage{
\method{anova}{tam}(object, \dots)
\method{logLik}{tam}(object, \dots)

\method{anova}{tam.mml}(object, \dots)
\method{logLik}{tam.mml}(object, \dots)

\method{anova}{tam.mml.3pl}(object, \dots)
\method{logLik}{tam.mml.3pl}(object, \dots)

\method{anova}{tamaan}(object, \dots)
\method{logLik}{tamaan}(object, \dots)

\method{anova}{tam.latreg}(object, \dots)
\method{logLik}{tam.latreg}(object, \dots)

\method{anova}{tam.np}(object, \dots)
\method{logLik}{tam.np}(object, \dots)
}



\arguments{
  \item{object}{
Object of class \code{\link{tam}}, \code{\link{tam.mml}},
\code{\link{tam.mml.3pl}}, \code{\link{tam.latreg}}, \code{\link{tam.np}},
or \code{\link{tamaan}}. Note that for \code{anova} two objects
(fitted models) must be provided.
}
\item{\dots}{Further arguments to be passed}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A data frame containing the likelihood ratio test statistic and
information criteria.
}


%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%% Alexander Robitzsch
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
#############################################################################
# EXAMPLE 1: Dichotomous data sim.rasch - 1PL vs. 2PL model
#############################################################################

data(data.sim.rasch)
# 1PL estimation
mod1 <- TAM::tam.mml(resp=data.sim.rasch)
logLik(mod1)
# 2PL estimation
mod2 <- TAM::tam.mml.2pl(resp=data.sim.rasch, irtmodel="2PL")
logLik(mod2)
# Model comparison
anova( mod1, mod2 )
  ##     Model   loglike Deviance Npars      AIC      BIC    Chisq df       p
  ##   1  mod1 -42077.88 84155.77    41 84278.77 84467.40 54.05078 39 0.05508
  ##   2  mod2 -42050.86 84101.72    80 84341.72 84709.79       NA NA      NA

\dontrun{
#############################################################################
# EXAMPLE 2: Dataset reading (sirt package): 1- vs. 2-dimensional model
#############################################################################

data(data.read, package="sirt")

# 1-dimensional model
mod1 <- TAM::tam.mml.2pl(resp=data.read )
# 2-dimensional model
mod2 <- TAM::tam.fa(resp=data.read, irtmodel="efa", nfactors=2,
             control=list(maxiter=150) )
# Model comparison
anova( mod1, mod2 )
  ##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
  ##   1    mod1 -1954.888 3909.777    24 3957.777 4048.809 76.66491 11  0
  ##   2    mod2 -1916.556 3833.112    35 3903.112 4035.867       NA NA NA
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Likelihood ratio test}
%% \keyword{anova}
%% \keyword{logLik}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
