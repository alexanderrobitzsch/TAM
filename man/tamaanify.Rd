%% File Name: tamaanify.Rd
%% File Version: 0.554

\name{tamaanify}
\alias{tamaanify}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Function for Parsing \pkg{TAM} Input
}

\description{
This function parses a so called \code{tammodel} which is a
string used for model estimation in \pkg{TAM}.
The function is based on the \pkg{lavaan} syntax and operates
at the extension \code{\link{lavaanify.IRT}}.
}


\usage{
tamaanify(tammodel, resp, tam.method=NULL, doparse=TRUE )
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{tammodel}{
String for model definition following the rules described in Details and
in Examples.
}
  \item{resp}{
Item response dataset
}
  \item{tam.method}{
One of the \pkg{TAM} methods \code{tam.mml}, \code{tam.mml.2pl}
or \code{tam.mml.3pl}.
}
\item{doparse}{Optional logical indicating whether \code{lavmodel}
should be parsed for \code{DO} statements.}
}

%% ordered latent class models (\code{"OLCA"}),

\details{
The model syntax \code{tammodel} consists of several sections.
Some of them are optional. \cr

\code{ANALYSIS:} \cr
Possible model types are unidimensional and multidimensional
item response models (\code{TYPE="TRAIT"}), latent class models
(\code{"LCA"}), located latent class models (\code{"LOCLCA"};
e.g. Formann, 1989; Bartolucci, 2007),
ordered latent class models (\code{"OLCA"}; only works for
dichotomous item responses; e.g. Hoijtink, 1997; Shojima, 2007) and
mixture distribution models (\code{"MIXTURE"}; e.g. von Davier, 2007). \cr

\code{LAVAAN MODEL:} \cr
For specification of the syntax, see \code{\link{lavaanify.IRT}}. \cr

\code{MODEL CONSTRAINT:} \cr
Linear constraints can be specified by using conventional
specification in \R syntax. All terms must be combined
with the \code{+} operator. Equality constraints are
set by using the \code{==} operator as in \pkg{lavaan}. \cr

\code{ITEM TYPE:} \cr
The following item types can be defined: Rasch model (\code{Rasch}),
the 2PL model (\code{2PL}), partial credit model (\code{PCM})
and the generalized partial credit model (\code{GPCM}).

The item intercepts can also be smoothed for the \code{PCM}
and the \code{GPCM} by using a Fourier basis proposed by
Thissen, Cai and Bock (2010). For an item with a maximum
of score of \eqn{K}, a smoothed partial credit model
is requested by \code{PCM(kk)} where \code{kk} is an
integer between 1 and \eqn{K}. With \code{kk}=1, only a linear
function is used. The subsequent integers correspond to
Fourier functions with decreasing periods.
See Example 2, Model 7 of the \code{\link{tamaan}}
function. \cr



\code{PRIOR:} \cr
Possible prior distributions: Normal distribution \code{N(mu,sd)},
truncated normal distribution \code{TN(mu,sd,low,upp)} and
Beta distribution \code{Beta(a,b)}.
Parameter labels and prior specification must be separated
by \code{~}.\cr


}


%% \emph{TO BE IMPLEMENTED: (ordered) latent class models
%% for multiple dimensions, cognitive diagnostic models} \cr

%\emph{TO BE IMPLEMENTED: ITEM TYPE beta item response model, nominal item response model,
%smoothed nominal item response model, nonparametric B-spline
%item response functions, cognitive diagnostic models,
%latent class approximation of factor models}


\value{
A list with following (optional) entries
 which are used as input in one of the \pkg{TAM} functions
 \code{\link{tam.mml}}, \code{\link{tam.mml.2pl}} or
 \code{\link{tam.mml.3pl}}:

\item{tammodel}{Model input for \pkg{TAM}}
\item{tammodel.dfr}{Processed \code{tammodel} input}
\item{ANALYSIS}{Syntax specified in \code{ANALYSIS}}
\item{ANALYSIS.list}{Parsed specifications in \code{ANALYSIS}}
\item{LAVAANMODEL}{Syntax specified in \code{LAVAAN MODEL}}
\item{lavpartable}{Parameter table processed by the
syntax in \code{LAVAAN MODEL}}
\item{items}{Informations about items: Number of categories,
  specified item response function}
\item{maxcat}{Maximum number of categories}
\item{ITEMTYPE}{Syntax specified in \code{ITEM TYPE}}
\item{MODELCONSTRAINT}{Syntax specified in \code{MODEL CONSTRAINT}}
\item{MODELCONSTRAINT.dfr}{Processed syntax in \code{MODEL CONSTRAINT}}
\item{modelconstraint.thresh}{Processed data frame for model constraint of thresholds}
\item{modelconstraint.loading}{Processed data frame for loadings}
\item{resp}{Data set for usage}
\item{method}{Used \pkg{TAM} function}
\item{A}{Design matrix A}
\item{Q}{Design matrix for loadings}
\item{Q.fixed}{Fixed values in \eqn{Q} matrix}
\item{B.fixed}{Matrix with fixed item loadings
      (used for \code{tam.mml.2pl})}
\item{L}{Processed design matrix for loadings when there
are model constraints for loadings}
\item{variance.fixed}{Matrix for specification of fixed values
in covariance matrix}
\item{est.variance}{Logical indicating whether variance should
be estimated (\code{\link{tam.mml.2pl}})
}
\item{theta.k}{Theta design matrix}
\item{E}{Design matrix E}
\item{notA}{Logical indicating whether \eqn{A} matrix is defined}
\item{gammaslope.fixed}{Fixed \code{gammaslope} parameters}
\item{gammaslope.prior}{Prior distributions for \code{gammaslope} parameters}
\item{xsi.fixed}{Fixed \eqn{\xi} parameter}
\item{xsi.prior}{Prior distributions for \eqn{\xi} parameters}
}

\references{
Bartolucci, F. (2007). A class of multidimensional IRT models for testing
unidimensionality and clustering items.
\emph{Psychometrika, 72}, 141-157.
\doi{10.1007/s11336-005-1376-9}

Formann, A. K. (1989). Constrained latent class models: Some further
applications. \emph{British Journal of Mathematical and Statistical
Psychology, 42}, 37-54.
\doi{10.1111/j.2044-8317.1989.tb01113.x}

Hojtink, H., & Molenaar, I. W. (1997). A multidimensional item response model:
Constrained latent class analysis using the Gibbs sampler and posterior
predictive checks. \emph{Psychometrika, 62}(2), 171-189.
\doi{10.1007/BF02295273}

Thissen, D., Cai, L., & Bock, R. D. (2010).
The nominal categories item response model.
In M. L. Nering & Ostini, R. (Eds.).
\emph{Handbook of Polytomous Item Response Models}
(pp. 43-75). New York: Routledge.

Shojima, K. (2007). \emph{Latent rank theory: Estimation of item reference
profile by marginal maximum likelihood method with EM algorithm}.
DNC Research Note 07-12.

von Davier, M. (2007). \emph{Mixture distribution diagnostic models}.
ETS Research Report ETS RR-07-32. Princeton, ETS.
\doi{10.1002/j.2333-8504.2007.tb02074.x}
}

%\author{
%%  ~~who you are~~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{tamaan}} for more examples. Other examples
are included in \code{\link{tam.mml}} and \code{\link{tam.mml.3pl}}.

\code{\link{lavaanify.IRT}}
}


\examples{
\dontrun{

#############################################################################
# EXAMPLE 1: Examples dichotomous data data.read
#############################################################################

library(sirt)
data(data.read,package="sirt")
dat <- data.read

#*********************************************************************
#*** Model 1: 2PL estimation with some fixed parameters and
#             equality constraints
tammodel <- "
LAVAAN MODEL:
  F2=~ C1__C2 + 1.3*C3 + C4
  F1=~ A1__B1
  # fixed loading of 1.4 for item B2
  F1=~ 1.4*B2
  F1=~ B3
  F1 ~~ F1
  F2 ~~ F2
  F1 ~~ F2
  B1 | 1.23*t1 ; A3 | 0.679*t1
  A2 | a*t1 ; C2 | a*t1 ; C4 | a*t1
  C3 | x1*t1 ; C1 | x1*t1
ITEM TYPE:
  A1__A3 (Rasch) ;
  A4 (2PL) ;
  B1__C4 (Rasch) ;
    "
# process model
out <- TAM::tamaanify( tammodel, resp=dat)
# inspect some output
out$method          # used TAM function
out$lavpartable     # lavaan parameter table

#*********************************************************************
#*** Model 2: Latent class analysis with three classes
tammodel <- "
ANALYSIS:
  TYPE=LCA;
  NCLASSES(3);   # 3 classes
  NSTARTS(5,20); # 5 random starts with 20 iterations
LAVAAN MODEL:
  F=~ A1__C4
    "
# process syntax
out <- TAM::tamaanify( tammodel, resp=dat)
str(out$E)     # E design matrix for estimation with tam.mml.3pl function

#*********************************************************************
#*** Model 3: Linear constraints for item intercepts and item loadings
tammodel <- "
LAVAAN MODEL:
  F=~ lam1__lam10*A1__C2
  F ~~ F
  A1 | a1*t1
  A2 | a2*t1
  A3 | a3*t1
  A4 | a4*t1
  B1 | b1*t1
  B2 | b2*t1
  B3 | b3*t1
  C1 | t1
MODEL CONSTRAINT:
  # defined parameters
  # only linear combinations are permitted
  b2==1.3*b1 + (-0.6)*b3
  a1==q1
  a2==q2 + t
  a3==q1 + 2*t
  a4==q2 + 3*t
  # linear constraints for loadings
  lam2==1.1*lam1
  lam3==0.9*lam1 + (-.1)*lam0
  lam8==lam0
  lam9==lam0
    "
# parse syntax
mod1 <- TAM::tamaanify( tammodel, resp=dat)
mod1$A          # design matrix A for intercepts
mod1$L[,1,]     # design matrix L for loadings
}

#############################################################################
# EXAMPLE 2: Examples polytomous data data.Students
#############################################################################

library(CDM)
data( data.Students, package="CDM")
dat <- data.Students[,3:13]

#*********************************************************************
#*** Model 1: Two-dimensional generalized partial credit model
tammodel <- "
LAVAAN MODEL:
  FA=~ act1__act5
  FS=~ sc1__sc4
  FA ~~ 1*FA
  FS ~~ 1*FS
  FA ~~ FS
  act1__act3 | t1
  sc2 | t2
    "
out <- TAM::tamaanify( tammodel, resp=dat)
out$A    # design matrix for item intercepts
out$Q    # loading matrix for items

#*********************************************************************
#*** Model 2: Linear constraints

# In the following syntax, linear equations for multiple constraints
# are arranged over multiple lines.
tammodel <- "
  LAVAAN MODEL:
    F=~ a1__a5*act1__act5
    F ~~ F
  MODEL CONSTRAINT:
      a1==delta +
                tau1
      a2==delta
      a3==delta + z1
      a4==1.1*delta +
              2*tau1
                + (-0.2)*z1
  "
# tamaanify model
res <- TAM::tamaanify( tammodel, dat )
res$MODELCONSTRAINT.dfr
res$modelconstraint.loading
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Model specification}
%% \keyword{TAM language}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
