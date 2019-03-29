%% File Name: tam.mml.Rd
%% File Version: 2.9861

\name{tam.mml}

\alias{tam}
\alias{tam.mml}
\alias{tam.mml.2pl}
\alias{tam.mml.mfr}
\alias{summary.tam}
\alias{summary.tam.mml}
\alias{print.tam}
\alias{print.tam.mml}
\alias{prior_list_include}

\title{
Test Analysis Modules: Marginal Maximum Likelihood Estimation
}
\description{
Modules for psychometric test analysis demonstrated
  with the help of artificial example data.
  The package includes MML and JML estimation of
  uni- and multidimensional IRT (Rasch,  2PL, Generalized Partial
  Credit, Rating Scale, Multi Facets, Nominal Item Response)
  models, fit statistic computation, standard error estimation, as
  well as plausible value imputation and weighted likelihood
  estimation of ability.
}

%  Wrapper and outlining methods (S3, wrapper, plot, print, summary)
%  are still to come.


\usage{
tam(resp, irtmodel="1PL", formulaA=NULL, ...)

tam.mml(resp, Y=NULL, group=NULL, irtmodel="1PL", formulaY=NULL,
    dataY=NULL, ndim=1,  pid=NULL, xsi.fixed=NULL, xsi.inits=NULL,
    beta.fixed=NULL, beta.inits=NULL, variance.fixed=NULL,
    variance.inits=NULL, est.variance=TRUE, constraint="cases", A=NULL,
    B=NULL, B.fixed=NULL,  Q=NULL,  est.slopegroups=NULL, E=NULL,
    pweights=NULL, userfct.variance=NULL,
    variance.Npars=NULL, item.elim=TRUE, verbose=TRUE, control=list() )

tam.mml.2pl(resp, Y=NULL, group=NULL, irtmodel="2PL", formulaY=NULL,
    dataY=NULL, ndim=1, pid=NULL, xsi.fixed=NULL, xsi.inits=NULL,
    beta.fixed=NULL, beta.inits=NULL, variance.fixed=NULL,
    variance.inits=NULL, est.variance=FALSE, A=NULL, B=NULL,
    B.fixed=NULL, Q=NULL, est.slopegroups=NULL, E=NULL, gamma.init=NULL,
    pweights=NULL, userfct.variance=NULL, variance.Npars=NULL,
    item.elim=TRUE, verbose=TRUE, control=list() )

tam.mml.mfr(resp, Y=NULL, group=NULL, irtmodel="1PL", formulaY=NULL,
    dataY=NULL, ndim=1, pid=NULL, xsi.fixed=NULL, xsi.setnull=NULL,
    xsi.inits=NULL, beta.fixed=NULL, beta.inits=NULL, variance.fixed=NULL,
    variance.inits=NULL, est.variance=TRUE, formulaA=~item+item:step,
    constraint="cases", A=NULL, B=NULL,  B.fixed=NULL, Q=NULL,
    facets=NULL, est.slopegroups=NULL, E=NULL,
    pweights=NULL, verbose=TRUE, control=list(), delete.red.items=TRUE )

\method{summary}{tam}(object, file=NULL, \dots)

\method{summary}{tam.mml}(object, file=NULL, \dots)

\method{print}{tam}(x, \dots)

\method{print}{tam.mml}(x, \dots)

}

% ## specification of prior distributions
% prior_list_include( prior_list, prior, index )


%% USAGE
%%tam.jml(resp, group=NULL, disattenuate=FALSE, bias=TRUE,
%%    xsi.fixed=NULL, xsi.inits=NULL,  A=NULL, B=NULL, Q=NULL,
%%    R=NULL, ndim=1, pweights=NULL, control=list())

%%tam.jml2(resp, group=NULL, disattenuate=FALSE, bias=TRUE,
%%    xsi.fixed=NULL, xsi.inits=NULL,  A=NULL, B=NULL,
%%    Q=NULL, R=NULL, ndim=1, pweights=NULL, control=list())

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ARGUMENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resp}{
     Data frame with polytomous item responses \eqn{k=0,...,K}.
     Missing responses must be declared as \code{NA}.
}
  \item{Y}{
     A matrix of covariates in latent regression. Note that the
     intercept is automatically included as the first predictor.
}
  \item{group}{
     An optional vector of group identifiers
}
  \item{irtmodel}{
     For fixed item slopes (in \code{tam.mml})
     options include \code{PCM} (partial credit model), \code{PCM2}
     (partial credit model with ConQuest parametrization
     \code{'item+item*step'} and \code{RSM} (rating scale model;
     the ConQuest parametrization \code{'item+step'}). \cr
     For estimated item slopes (only available in \code{tam.mml.2pl})
     options are \code{2PL} (all slopes of item categories
     are estimated; Nominal Item Response Model),
     \code{GPCM} (generalized partial credit
     model in which every item gets one and only slope
     parameter per dimension) and \code{2PL.groups}
     (subsets of items get same item slope estimates)
     and a design matrix \code{E} on item slopes in the
     generalized partial credit model (\code{GPCM.design},
     see Examples).
     Note that item slopes can not be estimated with faceted
     designs using the function \code{tam.mml.mfr}. However,
     it is easy to use pre-specified design matrices and apply
     some restrictions to \code{tam.mml.2pl} (see Example 14, Model 3).
}
  \item{formulaY}{
     An \R formula for latent regression. Transformations of predictors
     in \eqn{Y} (included in \code{dataY}) can be easily specified,
     e. g. \code{female*race} or \code{I(age^2)}.
}
  \item{dataY}{
     An optional data frame with possible covariates \eqn{Y} in latent regression.
     This data frame is used if an \R formula in \code{formulaY}
     is specified.
}
  \item{ndim}{
     Number of dimensions (is not needed to determined by the user)
}
  \item{pid}{
     An optional vector of person identifiers
}
  \item{xsi.fixed}{
     A matrix with two columns for fixing \eqn{\xi} parameters.
     1st column: index of \eqn{\xi} parameter, 2nd column: fixed value
}
\item{xsi.setnull}{A vector of strings indicating which \eqn{\xi}
    elements should be set to zero which do have entries in
    \code{xsi.setnull} in their labels (see Example 10a).}
  \item{xsi.inits}{
     A matrix with two columns (in the same way defined as in
     \code{xsi.fixed} with initial value for \eqn{\xi}.
}
  \item{beta.fixed}{
     A matrix with three columns for fixing regression coefficients.
     1st column: Index of \eqn{Y} value, 2nd column: dimension,
     3rd column: fixed \eqn{\beta} value. \cr
     If no constraints should be imposed on \eqn{\beta}, then
     set \code{beta.fixed=FALSE} (see Example 2, Model \code{2_4}).
}
  \item{beta.inits}{
     A matrix (same format as in \code{beta.fixed})
     with initial \eqn{\beta} values
}
  \item{variance.fixed}{
     An optional matrix with three columns for fixing
     entries in covariance matrix:
     1st column: dimension 1, 2nd column: dimension 2,
     3rd column: fixed value
}
  \item{variance.inits}{
     Initial covariance matrix in estimation. All matrix entries have to be
     specified and this matrix is NOT in the same format like
     \code{variance.fixed}.
}
  \item{est.variance}{
     Should the covariance matrix be estimated? This argument
     applies to estimated item slopes in \code{tam.mml.2pl}.
     The default is \code{FALSE} which means that latent
     variables (in the first group) are standardized in 2PL estimation.
}
\item{constraint}{Set sum constraint for parameter identification
    for \code{items} or \code{cases} (applies to
    \code{tam.mml} and \code{tam.mml.mfr})
    }
  \item{A}{
     An optional array of dimension \eqn{ I \times (K+1) \times N_\xi}.
     Only \eqn{\xi} parameters are estimated, entries in \eqn{A}
     only correspond to the design.
}
  \item{B}{
     An optional array of dimension \eqn{ I \times (K+1) \times D}.
     In case of \code{tam.mml.2pl} entries of the \eqn{B} matrix can be
     estimated.
}
  \item{B.fixed}{
  An optional matrix with four columns for fixing \eqn{B} matrix entries in 2PL estimation.
  1st column: item index, 2nd column: category, 3rd column: dimension,
  4th column: fixed value.
}
  \item{Q}{
  An optional \eqn{I \times D} matrix (the Q-matrix) which specifies the
  loading structure of items on dimensions.
}
  \item{est.slopegroups}{
  A vector of integers of length \eqn{I} for estimating item slope
  parameters of item groups. This function only applies to
  the generalized partial credit model \cr (\code{irtmodel="2PL.groups"}).
}
\item{E}{
  An optional design matrix for estimating item slopes
  in the generalized partial credit model (\code{irtmodel="GPCM.design"})
}
\item{gamma.init}{Optional initial \eqn{gamma} parameter vector
(\code{irtmodel="GPCM.design"}).
}
\item{pweights}{
     An optional vector of person weights
}
\item{formulaA}{Design formula (only applies to \code{tam.mml.mfr}).
    See Example 8. It is also to possible to set all effects
  of a facet to zero, e.g. \code{item*step + 0*rater} (see Example 10a).}

\item{facets}{A data frame with facet entries (only applies to
    \code{tam.mml.mfr})
        }
\item{userfct.variance}{Optional user customized function for variance specification
(See Simulated Example 17).}
\item{variance.Npars}{Number of estimated parameters of variance matrix
if a \code{userfct.variance} is provided.}
\item{item.elim}{Optional logical indicating whether an item with has
    only zero entries should be removed from the analysis. The default
    is \code{TRUE}.}
\item{verbose}{Logical indicating whether output should
be printed during iterations. This argument replaces \code{control$progress}.
}
\item{control}{
  A list of control arguments for the algorithm: \cr\cr
  \code{ list( nodes=seq(-6,6,len=21), snodes=0, QMC=TRUE,}\cr
    \code{        convD=.001,conv=.0001, convM=.0001, Msteps=4,   }\cr
    \code{        maxiter=1000, max.increment=1, }\cr
    \code{        min.variance=.001, progress=TRUE, ridge=0,    }\cr
    \code{        seed=NULL, xsi.start0=0,, increment.factor=1,  }\cr
    \code{        fac.oldxsi=0, acceleration="none", dev_crit="absolute" )  }\cr
    \code{        trim_increment="half" )  }\cr

\code{nodes}: the discretized \eqn{\theta} nodes for numerical integration

\code{snodes}: number of simulated \eqn{\theta} nodes for stochastic integration.
    If \code{snodes=0}, numerical integration is used.

\code{QMC}: A logical indicating whether quasi Monte Carlo integration
   (Gonzales at al., 2006; Pan & Thompson, 2007) should be used. The default is \code{TRUE}.
Quasi Monte Carlo integration is a nonstochastic integration approach
but prevents the very demanding numeric integration using Gaussian quadrature.
In case of \code{QMC=FALSE}, "ordinary" stochastic integration is used
(see the section \emph{Integration} in Details).


\code{convD}: Convergence criterion for deviance

\code{conv}: Convergence criterion for item parameters and regression
coefficients

\code{convM}: Convergence criterion for item parameters within each
M step

\code{Msteps}: Number of M steps for item parameter estimation.
A high value of M steps could be helpful in cases of non-convergence.
In \code{tam.mml}, \code{tam.mml.2pl} and \code{tam.mml.mfr}, the default is
set to 4, in \code{tam.mml.3pl} it is set to 10.

\code{maxiter}: Maximum number of iterations

\code{max.increment}: Maximum increment for item parameter change for
every iteration

\code{min.variance}: Minimum variance to be estimated during iterations.

\code{progress}: A logical indicating whether computation progress should
be displayed at \R console

\code{ridge}: A numeric value or a vector of ridge parameter(s)
for the latent regression which is added to the covariance matrix
\eqn{Y'Y} of predictors in the diagonal.

\code{seed}: An optional integer defining the simulation seed
(important for reproducible results for stochastic integration)

\code{xsi.start0}: A numeric value. The value of 0 indicates that for all
parameters starting values are provided. A value of 1 means that all
starting values are set to zero and a value of 2 means that only
starting values of item parameters (but not facet parameters)
are used.

\code{increment.factor}: A value (larger than one)
    which defines the extent of the decrease of the maximum
    increment of item parameters in every iteration. The maximum increment
    in iteration \code{iter} is defined as
    \code{max.increment*increment.factor^(-iter)}
    where \code{max.increment=1}. Using a value larger than 1 helps
        to reach convergence in some non-converging analyses (see Example 12).

\code{fac.oldxsi}: An optional numeric value \eqn{f} between 0 and 1 which
defines the weight of parameter values in previous iteration. If \eqn{\xi_t}
denotes a parameter update in iteration \eqn{t}, \eqn{\xi_{t-1}} is the
parameter value of iteration \eqn{t-1}, then the modified parameter value
is defined as \eqn{ \xi_t^*=(1-f) \cdot \xi_t + f \cdot \xi_{t-1}}.
Especially in cases where the deviance increases, setting the parameter
larger than 0 (maybe .4 or .5) is helpful in stabilizing the algorithm
(see Example 15).

\code{acceleration}: String indicating whether convergence acceleration of
the EM algorithm should be employed. Options are \code{"none"} (no acceleration,
the default), the monotone overrelaxation method of \code{"Yu"} (Yu, 2012) and
\code{"Ramsay"} for the Ramsay (1975) acceleration method.

\code{dev_crit}: Criterion for convergence in deviance. \code{dev_crit="abolute"}
refers to absolute differences in successive deviance values, while
\code{dev_crit="relative"} refers to relative differences.

\code{trim_increment}: Type of method for trimming parameter increments in
algorithm. Possible types are \code{"half"} or "\code{"cut"}.
\cr


}
\item{delete.red.items}{An optional logical indicating whether redundant
    generalized items (with no observations) should be eliminated.
        }
  \item{object}{
Object of class \code{tam} or \code{tam.mml} (only for \code{summary.tam}
functions)
}
\item{file}{
A file name in which the summary output should be written
(only for \code{summary.tam} functions)
    }
\item{\dots}{
    Further arguments to be passed
    }
\item{x}{Object of class \code{tam} or \code{tam.mml}}
}

% \item{prior_list_xsi}{Optional list of prior distributions. See Example 1.4
% for specification and the function \code{prior_list_include}
% for convenient specification.}

%\item{prior_list}{List with prior distributions}
%\item{prior}{Univariate prior distribution}
%\item{index}{Parameter for which \code{prior} should be chosen}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DETAILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\details{
The multidimensional item response model in \pkg{TAM} is
described in Adams, Wilson and Wu (1997) or Adams and Wu (2007).

The data frame \code{resp} contains item responses of \eqn{N} persons (in rows)
at \eqn{I} items (in columns), each item having at most
\eqn{K} categories \eqn{k=0,...,K}.
The item response model has \eqn{D} dimensions of the \eqn{\theta} ability
vector and can be written as
\deqn{
        P( X_{pi}=k | \theta_p )  \propto exp( b_{ik} \theta_p + a_{ik} \xi )
        }
The symbol \eqn{\propto } means that response probabilities are normalized such
that \eqn{ \sum _k P( X_{pi}=k | \theta_p )=1 }.

Item category thresholds for item \eqn{i} in category \eqn{k}
are written as a linear combination
\eqn{a_i \xi} where the vector \eqn{\xi} of length \eqn{N_\xi}
contains generalized item parameters and
\eqn{A=( a_{ik} )_{ik}=( a_i )_{i} } is
a three-dimensional design array (specified in \code{A}).

The scoring vector \eqn{b_{ik}} contains the fixed (in \code{tam.mml})
or estimated (in \code{tam.mml.2pl}) scores of item \eqn{i} in category \eqn{k}
on dimension \eqn{d}.

For \code{tam.mml.2pl} and \code{irtmodel="GPCM.design"}, item slopes \eqn{a_i}
can be written as a linear combination \eqn{a_i=( E \gamma)_i }
of basis item slopes which is an analogue
of the LLTM for item slopes (see Example 7; Embretson, 1999).

The latent regression model regresses the latent trait \eqn{\theta_p}
on covariates \eqn{Y} which results in
\deqn{
    \theta_p=Y \beta + \epsilon_p, \epsilon_p \sim N_D ( 0, \Sigma )
}{%
  \theta_p=Y \beta + \epsilon_p, \epsilon_p ~ N_D ( 0, \Sigma )
    }

Where \eqn{\beta} is a \eqn{N_Y} times \eqn{D} matrix of regression
coefficients for \eqn{N_Y} covariates in \eqn{Y}.

The multiple group model for groups
\eqn{g=1,...,G} is implemented for unidimensional and multidimensional item
response models. In this case, variance heterogeneity is allowed
\deqn{
    \theta_p=Y \beta + \epsilon_p, \epsilon_p \sim N ( 0, \sigma_g^2 )
}{%
  \theta_p=Y \beta + \epsilon_p, \epsilon_p ~ N( 0, \sigma_g^2 )
    }

\bold{Integration}: Uni- and multidimensional integrals are approximated by
posing a uni- or multivariate normality assumption. The default is Gaussian
quadrature with nodes defined in \code{control$nodes}. For \eqn{D}-dimensional
IRT models, the \eqn{D}-dimensional cube consisting of the vector
\code{control$nodes} in all dimensions is used. If the user specifies
\code{control$snodes} with a value larger than zero, then Quasi-Monte Carlo
integration (Pan & Thomas, 2007; Gonzales et al., 2006) with \code{control$snodes} is used
(because \code{control$QMC=TRUE} is set by default). If
\code{control$QMC=FALSE} is specified, then stochastic (Monte Carlo) integration
is employed with \code{control$snodes} stochastic nodes.
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VALUES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\value{
A list with following entries:

\item{xsi}{Vector of \eqn{\xi} parameter estimates and their
corresponding standard errors}
\item{xsi.facets}{Data frame of \eqn{\xi} parameters and corresponding
constraints for multifacet models}
\item{beta}{Matrix of \eqn{\beta} regression coefficient estimates}
\item{variance}{Covariance matrix. In case of multiple groups,
    it is a vector indicating heteroscedastic variances}
\item{item}{Data frame with item parameters. The column \code{xsi.item}
denotes the item difficulty of polytomous items in the parametrization
\code{irtmodel="PCM2"}.}
\item{item_irt}{IRT parameterization of item parameters}
\item{person}{Matrix with person parameter estimates.
    \code{EAP} is the mean of the posterior distribution and \code{SD.EAP}
    the corresponding standard deviation
        }
\item{pid}{Vector of person identifiers}
\item{EAP.rel}{EAP reliability}
\item{post}{Posterior distribution for item response pattern}
\item{rprobs}{A three-dimensional array with estimated response probabilities
        (dimensions are items \eqn{\times} categories \eqn{\times}
      theta length)}
\item{itemweight}{Matrix of item weights}
\item{theta}{Theta grid}
\item{n.ik}{Array of expected counts: theta class \eqn{\times}
item \eqn{\times} category \eqn{\times} group}
\item{pi.k}{Marginal trait distribution at grid \code{theta}}
\item{Y}{Matrix of covariates}
\item{resp}{Original data frame}
\item{resp.ind}{Response indicator matrix}
\item{group}{Group identifier}
\item{G}{Number of groups}
\item{formulaY}{Formula for latent regression}
\item{dataY}{Data frame for latent regression}
\item{pweights}{Person weights}
\item{time}{Computation time}
\item{A}{Design matrix \eqn{A} for \eqn{\xi} parameters}
\item{B}{Fixed or estimated loading matrix}
\item{se.B}{Standard errors of \eqn{B} parameters}
\item{nitems}{Number of items}
\item{maxK}{Maximum number of categories}
\item{AXsi}{Estimated item intercepts \eqn{ a_{ik} \xi}}
\item{AXsi_}{Estimated item intercepts -\eqn{ a_{ik} \xi}.
Note that in \code{summary.tam}, the parameters \code{AXsi_}
are displayed.
}
\item{se.AXsi}{Standard errors of \eqn{a_{ik} \xi} parameters}
\item{nstud}{Number of persons}
\item{resp.ind.list}{List of response indicator vectors}
\item{hwt}{Individual posterior distribution}
\item{like}{Individual likelihood}
\item{ndim}{Number of dimensions}
\item{xsi.fixed}{Fixed \eqn{\xi} parameters}
\item{xsi.fixed.estimated}{Matrix of estimated \eqn{\xi} parameters
    in form of \code{xsi.fixed} which can
    be used for parameter fixing in subsequent estimations.}
\item{B.fixed}{Fixed loading parameters (only applies to \code{tam.mml.2pl})}
\item{B.fixed.estimated}{Matrix of estimated \eqn{B} parameters
   in the same format as \code{B.fixed}.}
\item{est.slopegroups}{An index vector of item groups of common slope
parameters (only applies to \code{tam.mml.2pl})}
\item{E}{Design matrix for estimated item slopes in the generalized partial
credit model (only applies to \code{tam.mml.2pl} in case of
\code{irtmodel="GPCM.design"})}
\item{basispar}{Vector of \eqn{\gamma} parameters of the linear combination
\eqn{a_i=( E \gamma)_i } for item slopes (only applies to
\code{tam.mml.2pl} in case of \code{irtmodel='GPCM.design'})}
\item{formulaA}{Design formula (only applies to \code{tam.mml.mfr})}
\item{facets}{Data frame with facet entries (only applies to
  \code{tam.mml.mfr})
}
\item{variance.fixed}{Fixed covariance matrix}
\item{nnodes}{Number of theta nodes}
\item{deviance}{Final deviance}
\item{ic}{Vector with information criteria}
\item{deviance.history}{Deviance history in iterations}
\item{control}{List of control arguments}
\item{latreg_stand}{List containing standardized regression coefficients}
\item{\dots}{Further values}
}

% \item{design}{Output produced by \code{designMatrices} or
% \code{designMatrices.mfr}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REFERENCES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\references{
Adams, R. J., Wilson, M., & Wu, M. (1997).
Multilevel item response models: An approach to errors in
variables regression. \emph{Journal of Educational and Behavioral
Statistics, 22}, 47-76.

Adams, R. J., & Wu, M. L. (2007). The mixed-coefficients multinomial logit model.
A generalized form of the Rasch model. In M. von Davier & C. H. Carstensen (Eds.):
\emph{Multivariate and mixture distribution Rasch models: Extensions and applications}
(pp. 55-76). New York: Springer.

Embretson, S. E. (1999). Generating items during testing:
Psychometric issues and models. \emph{Psychometrika, 64}, 407-433.

Gonzalez, J., Tuerlinckx, F., De Boeck, P., & Cools, R. (2006).
Numerical integration in logistic-normal models.
\emph{Computational Statistics & Data Analysis, 51}, 1535-1548.

Pan, J., & Thompson, R. (2007). Quasi-Monte Carlo estimation in
generalized linear mixed models. \emph{Computational Statistics &
Data Analysis, 51}, 5765-5775.

Ramsay, J. O. (1975). Solving implicit equations in psychometric data analysis.
\emph{Psychometrika, 40}(3), 337-360.

Yu, Y. (2012). Monotonically overrelaxed EM algorithms.
\emph{Journal of Computational and Graphical Statistics, 21}(2), 518-537.

Wu, M. L., Adams, R. J., Wilson, M. R. & Haldane, S. (2007).
\emph{ACER ConQuest Version 2.0}. Mulgrave.
https://shop.acer.edu.au/acer-shop/group/CON3.
}

%\author{
%\pkg{TAM} authors
%}

\note{
For more than three dimensions, quasi-Monte Carlo or stochastic integration
is recommended because otherwise problems in memory allocation and large
computation time will result. Choose in \code{control} a suitable value of
the number of quasi Monte Carlo or stochastic nodes \code{snodes}
(say, larger than 1000). See Pan and Thompson (2007) or Gonzales et al. (2006)
for more details.

In faceted models (\code{tam.mml.mfr}), parameters which cannot be estimated
are fixed to \code{99}.

Several choices can be made if your model does not converge. First, the number
of iterations within a M step can be increased (\code{Msteps=10}).
Second, the absolute value of increments can be forced with increasing
iterations (set a value higher than 1 to \code{max.increment}, maybe 1.05).
Third, change in estimated parameters can be stabilized by \code{fac.oldxsi} for
which a value of 0 (the default) and a value of 1 can be chosen. We recommend
value between .5 and .8 if your model does not converge.
}

%\section{Warning}{
%Be cautious when using the number of estimated parameters in \code{ic}.
%Extensive checking of errors has not be done yet.
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{data.cqc01}} for more examples which is are similar to the ones
in the ConQuest manual (Wu, Adams, Wilson & Haldane, 2007).

See \code{\link{tam.jml}} for joint maximum likelihood estimation.

Standard errors are estimated by a rather crude (but quick) approximation.
Use \code{\link{tam.se}} for improved standard errors.

For model comparisons see \code{\link{anova.tam}}.

See \code{\link[sirt:tam2mirt]{sirt::tam2mirt}} for converting
\code{tam} objects into objects of class
\code{\link[mirt:mirt]{mirt::mirt}} in the \pkg{mirt} package.
}

%% \code{\link{logLik.tam}}, \code{\link{anova.tam}}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\examples{
#############################################################################
# EXAMPLE 1: dichotomous data
# data.sim.rasch: 2000 persons, 40 items
#############################################################################
data(data.sim.rasch)

#************************************************************
# Model 1: Rasch model (MML estimation)
mod1 <- TAM::tam.mml(resp=data.sim.rasch)
# extract item parameters
mod1$item    # item difficulties

\dontrun{
# WLE estimation
wle1 <- TAM::tam.wle( mod1 )
# item fit
fit1 <- TAM::tam.fit(mod1)
# plausible value imputation
pv1 <- TAM::tam.pv(mod1, normal.approx=TRUE, ntheta=300)
# standard errors
se1 <- TAM::tam.se( mod1 )
# summary
summary(mod1)

#-- specification with tamaan
tammodel <- "
 LAVAAN MODEL:
   F=~ I1__I40;
   F ~~ F
 ITEM TYPE:
   ALL(Rasch)
   "
mod1t <- TAM::tamaan( tammodel, data.sim.rasch)
summary(mod1t)

#************************************************************
# Model 1a: Rasch model with fixed item difficulties from 'mod1'
xsi0 <- mod1$xsi$xsi
xsi.fixed <- cbind( 1:(length(xsi0)), xsi0 )
        # define vector with fixed item difficulties
mod1a <- TAM::tam.mml( resp=data.sim.rasch, xsi.fixed=xsi.fixed )
summary(mod1a)

# Usage of the output value mod1$xsi.fixed.estimated has the right format
# as the input of xsi.fixed
mod1aa <- TAM::tam.mml( resp=data.sim.rasch, xsi.fixed=mod1$xsi.fixed.estimated )
summary(mod1b)

#************************************************************
# Model 1b: Rasch model with initial xsi parameters for items 2 (item difficulty b=-1.8),
# item 4 (b=-1.6) and item 40 (b=2)
xsi.inits <- cbind( c(2,4,40), c(-1.8,-1.6,2))
mod1b <- TAM::tam.mml( resp=data.sim.rasch, xsi.inits=xsi.inits )

#-- tamaan specification
tammodel <- "
 LAVAAN MODEL:
   F=~ I1__I40
   F ~~ F
   # Fix item difficulties. Note that item intercepts instead of difficulties
   # must be specified.
   I2 | 1.8*t1
   I4 | 1.6*t1
 ITEM TYPE:
   ALL(Rasch)
   "
mod1bt <- TAM::tamaan( tammodel, data.sim.rasch)
summary(mod1bt)

#************************************************************
# Model 1c: 1PL estimation with sum constraint on item difficulties
dat <- data.sim.rasch
# modify A design matrix to include the sum constraint
des <- TAM::designMatrices(resp=dat)
A1 <- des$A[,, - ncol(dat) ]
A1[ ncol(dat),2, ] <- 1
A1[,2,]
# estimate model
mod1c <- TAM::tam.mml( resp=dat, A=A1, beta.fixed=FALSE,
           control=list(fac.oldxsi=.1) )
summary(mod1c)

#************************************************************
# Model 1d: estimate constraint='items' using tam.mml.mfr
formulaA=~ 0 + item
mod1d <- TAM::tam.mml.mfr( resp=dat, formulaA=formulaA,
                     control=list(fac.oldxsi=.1), constraint="items")
summary(mod1d)

#************************************************************
# Model 1e: This sum constraint can also be obtained by using the argument
# constraint="items" in tam.mml
mod1e <- TAM::tam.mml( resp=data.sim.rasch, constraint="items" )
summary(mod1e)

#************************************************************
# Model 1d2: estimate constraint='items' using tam.mml.mfr
# long format response data
resp.long <- c(dat)

# pid and item facet specifications are necessary
#     Note, that we recommend the facet labels to be sortable in the same order that the
#     results are desired.
#     compare to: facets <- data.frame( "item"=rep(colnames(dat), each=nrow(dat)) )
pid <- rep(1:nrow(dat), ncol(dat))
itemnames <- paste0("I", sprintf(paste('\%0', max(nchar(1:ncol(dat))), 'i', sep='' ),
                    c(1:ncol(dat)) ) )
facets   <- data.frame( "item_"=rep(itemnames, each=nrow(dat)) )
formulaA=~ 0 + item_

mod1d2 <- TAM::tam.mml.mfr( resp=resp.long, formulaA=formulaA, control=list(fac.oldxsi=.1),
                       constraint="items", facets=facets, pid=pid)
stopifnot( all(mod1d$xsi.facets$xsi==mod1d2$xsi.facets$xsi) )
}


#************************************************************
# Model 2: 2PL model
mod2 <- TAM::tam.mml.2pl(resp=data.sim.rasch,irtmodel="2PL")

# extract item parameters
mod2$xsi    # item difficulties
mod2$B      # item slopes

#--- tamaan specification
tammodel <- "
 LAVAAN MODEL:
   F=~ I1__I40
   F ~~ 1*F
   # item type of 2PL is the default for dichotomous data
   "
# estimate model
mod2t <- TAM::tamaan( tammodel, data.sim.rasch)
summary(mod2t)

\dontrun{
#************************************************************
# Model 2a: 2PL with fixed item difficulties and slopes from 'mod2'
xsi0 <- mod2$xsi$xsi
xsi.fixed <- cbind( 1:(length(xsi0)), xsi0 )
        # define vector with fixed item difficulties
mod2a <- TAM::tam.mml( resp=data.sim.rasch, xsi.fixed=xsi.fixed,
                 B=mod2$B # fix slopes
            )
summary(mod2a)
mod2a$B     # inspect used slope matrix

#************************************************************
# Model 3: constrained 2PL estimation
# estimate item parameters in different slope groups
# items 1-10, 21-30 group 1
# items 11-20 group 2 and items 31-40 group 3
est.slope <- rep(1,40)
est.slope[ 11:20 ] <- 2
est.slope[ 31:40 ] <- 3
mod3 <- TAM::tam.mml.2pl( resp=data.sim.rasch, irtmodel="2PL.groups",
               est.slopegroups=est.slope )
mod3$B
summary(mod3)

#--- tamaan specification (A)
tammodel <- "
 LAVAAN MODEL:
   F=~ lam1*I1__I10 + lam2*I11__I20 + lam1*I21__I30 + lam3*I31__I40;
   F ~~ 1*F
   "
# estimate model
mod3tA <- TAM::tamaan( tammodel, data.sim.rasch)
summary(mod3tA)

#--- tamaan specification (alternative B)
tammodel <- "
 LAVAAN MODEL:
   F=~ a1__a40*I1__I40;
   F ~~ 1*F
 MODEL CONSTRAINT:
   a1__a10==lam1
   a11__a20==lam2
   a21__a30==lam1
   a31__a40==lam3
   "
mod3tB <- TAM::tamaan( tammodel, data.sim.rasch)
summary(mod3tB)

#--- tamaan specification (alternative C using DO operator)
tammodel <- "
 LAVAAN MODEL:
 DO(1,10,1)
   F=~ lam1*I\%
 DOEND
 DO(11,20,1)
   F=~ lam2*I\%
 DOEND
 DO(21,30,1)
   F=~ lam1*I\%
 DOEND
 DO(31,40,1)
   F=~ lam3*I\%
 DOEND
   F ~~ 1*F
   "
# estimate model
mod3tC <- TAM::tamaan( tammodel, data.sim.rasch)
summary(mod3tC)

#############################################################################
# EXAMPLE 2: Unidimensional calibration with latent regressors
#############################################################################

# (1) simulate data
set.seed(6778)     # set simulation seed
N <- 2000          # number of persons
# latent regressors Y
Y <- cbind( stats::rnorm( N, sd=1.5), stats::rnorm(N, sd=.3 ) )
# simulate theta
theta <- stats::rnorm( N ) + .4 * Y[,1] + .2 * Y[,2]  # latent regression model
# number of items
I <- 40
p1 <- stats::plogis( outer( theta, seq( -2, 2, len=I ), "-" ) )
# simulate response matrix
resp <- 1 * ( p1 > matrix( stats::runif( N*I ), nrow=N, ncol=I ) )
colnames(resp) <- paste("I", 1:I, sep="")

# (2) estimate model
mod2_1 <- TAM::tam.mml(resp=resp, Y=Y)
summary(mod2_1)

# (3) setting initial values for beta coefficients
#   beta_2=.20, beta_3=.35 for dimension 1
beta.inits <- cbind( c(2,3), 1, c(.2, .35 ) )
mod2_2 <- TAM::tam.mml(resp=resp, Y=Y, beta.inits=beta.inits)

# (4) fix intercept to zero and third coefficient to .3
beta.fixed <- cbind( c(1,3), 1, c(0, .3 ) )
mod2_3 <- TAM::tam.mml(resp=resp, Y=Y, beta.fixed=beta.fixed )

# (5) same model but with R regression formula for Y
dataY <- data.frame(Y)
colnames(dataY) <- c("Y1","Y2")
mod2_4 <- TAM::tam.mml(resp=resp, dataY=dataY, formulaY=~ Y1+Y2 )
summary(mod2_4)

# (6) model with interaction of regressors
mod2_5 <- TAM::tam.mml(resp=resp, dataY=dataY, formulaY=~ Y1*Y2 )
summary(mod2_5)

# (7) no constraint on regressors (removing constraint from intercept)
mod2_6 <- TAM::tam.mml(resp=resp, Y=Y, beta.fixed=FALSE )

#############################################################################
# EXAMPLE 3: Multiple group estimation
#############################################################################

# (1) simulate data
set.seed(6778)
N <- 3000
theta <- c( stats::rnorm(N/2,mean=0,sd=1.5), stats::rnorm(N/2,mean=.5,sd=1)  )
I <- 20
p1 <- stats::plogis( outer( theta, seq( -2, 2, len=I ), "-" ) )
resp <- 1 * ( p1 > matrix( stats::runif( N*I ), nrow=N, ncol=I ) )
colnames(resp) <- paste("I", 1:I, sep="")
group <- rep(1:2, each=N/2 )

# (2) estimate model
mod3_1 <- TAM::tam.mml( resp,  group=group )
summary(mod3_1)

#############################################################################
# EXAMPLE 4: Multidimensional estimation
# with two dimensional theta's - simulate some bivariate data,
# and regressors
# 40 items: first 20 items load on dimension 1,
#           second 20 items load on dimension 2
#############################################################################

# (1) simulate some data
set.seed(6778)
library(mvtnorm)
N <- 1000
Y <- cbind( stats::rnorm( N ), stats::rnorm(N) )
theta <- mvtnorm::rmvnorm( N,mean=c(0,0), sigma=matrix( c(1,.5,.5,1), 2, 2 ))
theta[,1] <- theta[,1] + .4 * Y[,1] + .2 * Y[,2]  # latent regression model
theta[,2] <- theta[,2] + .8 * Y[,1] + .5 * Y[,2]  # latent regression model
I <- 20
p1 <- stats::plogis( outer( theta[,1], seq( -2, 2, len=I ), "-" ) )
resp1 <- 1 * ( p1 > matrix( stats::runif( N*I ), nrow=N, ncol=I ) )
p1 <- stats::plogis( outer( theta[,2], seq( -2, 2, len=I ), "-" ) )
resp2 <- 1 * ( p1 > matrix( stats::runif( N*I ), nrow=N, ncol=I ) )
resp <- cbind(resp1,resp2)
colnames(resp) <- paste("I", 1:(2*I), sep="")

# (2) define loading Matrix
Q <- array( 0, dim=c( 2*I, 2 ))
Q[cbind(1:(2*I), c( rep(1,I), rep(2,I) ))] <- 1

# (3) estimate models

#************************************************************
# Model 4.1: Rasch model: without regressors
mod4_1 <- TAM::tam.mml( resp=resp, Q=Q )

#--- tamaan specification
tammodel <- "
  LAVAAN MODEL:
    F1=~ 1*I1__I20
    F2=~ 1*I21__I40
    # Alternatively to the factor 1 one can use the item type Rasch
    F1 ~~ F1
    F2 ~~ F2
    F1 ~~ F2
    "
mod4_1t <- TAM::tamaan( tammodel, resp, control=list(maxiter=100))
summary(mod4_1t)

#************************************************************
# Model 4.1b: estimate model with sum constraint of items for each dimension
mod4_1b <- TAM::tam.mml( resp=resp, Q=Q,constraint="items")

#************************************************************
# Model 4.2: Rasch model: set covariance between dimensions to zero
variance_fixed <- cbind( 1, 2, 0 )
mod4_2 <- TAM::tam.mml( resp=resp, Q=Q, variance.fixed=variance_fixed )
summary(mod4_2)

#--- tamaan specification
tammodel <- "
  LAVAAN MODEL:
    F1=~ I1__I20
    F2=~ I21__I40
    F1 ~~ F1
    F2 ~~ F2
    F1 ~~ 0*F2
  ITEM TYPE:
    ALL(Rasch)
    "
mod4_2t <- TAM::tamaan( tammodel, resp)
summary(mod4_2t)

#************************************************************
# Model 4.3: 2PL model
mod4_3 <- TAM::tam.mml.2pl( resp=resp, Q=Q, irtmodel="2PL" )

#--- tamaan specification
tammodel <- "
  LAVAAN MODEL:
    F1=~ I1__I20
    F2=~ I21__I40
    F1 ~~ F1
    F2 ~~ F2
    F1 ~~ F2
    "
mod4_3t <- TAM::tamaan( tammodel, resp )
summary(mod4_3t)

#************************************************************
# Model 4.4: Rasch model with 2000 quasi monte carlo nodes
# -> nodes are useful for more than 3 or 4 dimensions
mod4_4 <- TAM::tam.mml( resp=resp, Q=Q, control=list(snodes=2000) )

#************************************************************
# Model 4.5: Rasch model with 2000 stochastic nodes
mod4_5 <- TAM::tam.mml( resp=resp, Q=Q,control=list(snodes=2000,QMC=FALSE))

#************************************************************
# Model 4.6: estimate two dimensional Rasch model with regressors
mod4_6 <- TAM::tam.mml( resp=resp, Y=Y, Q=Q )

#--- tamaan specification
tammodel <- "
  LAVAAN MODEL:
    F1=~ I1__I20
    F2=~ I21__I40
    F1 ~~ F1
    F2 ~~ F2
    F1 ~~ F2
  ITEM TYPE:
    ALL(Rasch)
    "
mod4_6t <- TAM::tamaan( tammodel, resp, Y=Y )
summary(mod4_6t)

#############################################################################
# EXAMPLE 5: 2-dimensional estimation with within item dimensionality
#############################################################################
library(mvtnorm)
# (1) simulate data
set.seed(4762)
N <- 2000 # 2000 persons
Y <- stats::rnorm( N )
theta <- mvtnorm::rmvnorm( N,mean=c(0,0), sigma=matrix( c(1,.5,.5,1), 2, 2 ))
I <- 10
# 10 items load on the first dimension
p1 <- stats::plogis( outer( theta[,1], seq( -2, 2, len=I ), "-" ) )
resp1 <- 1 * ( p1 > matrix( stats::runif( N*I ), nrow=N, ncol=I ) )
# 10 items load on the second dimension
p1 <- stats::plogis( outer( theta[,2], seq( -2, 2, len=I ), "-" ) )
resp2 <- 1 * ( p1 > matrix( stats::runif( N*I ), nrow=N, ncol=I ) )
# 20 items load on both dimensions
p1 <- stats::plogis( outer( 0.5*theta[,1] + 1.5*theta[,2], seq(-2,2,len=2*I ), "-" ))
resp3 <- 1 * ( p1 > matrix( stats::runif( N*2*I ), nrow=N, ncol=2*I ) )
#Combine the two sets of items into one response matrix
resp <- cbind(resp1, resp2, resp3 )
colnames(resp) <- paste("I", 1:(4*I), sep="")

# (2) define loading matrix
Q <- cbind(c(rep(1,10),rep(0,10),rep(1,20)), c(rep(0,10),rep(1,10),rep(1,20)))

# (3) model: within item dimensionality and 2PL estimation
mod5 <- TAM::tam.mml.2pl(resp, Q=Q, irtmodel="2PL" )
summary(mod5)

# item difficulties
mod5$item
# item loadings
mod5$B

#--- tamaan specification
tammodel <- "
  LAVAAN MODEL:
    F1=~ I1__I10 + I21__I40
    F2=~ I11__I20 + I21__I40
    F1 ~~ 1*F1
    F1 ~~ F2
    F2 ~~ 1*F2
    "
mod5t <- TAM::tamaan( tammodel, resp,  control=list(maxiter=10))
summary(mod5t)

#############################################################################
# EXAMPLE 6: ordered data - Generalized partial credit model
#############################################################################
data(data.gpcm, package="TAM")

#************************************************************
# Ex6.1: Nominal response model (irtmodel="2PL")
mod6_1 <- TAM::tam.mml.2pl( resp=data.gpcm, irtmodel="2PL", control=list(maxiter=200) )
mod6_1$item # item intercepts
mod6_1$B    # for every category a separate slope parameter is estimated

# reestimate the model with fixed item parameters
mod6_1a <- TAM::tam.mml.2pl( resp=data.gpcm, irtmodel="2PL",
       xsi.fixed=mod6_1$xsi.fixed.estimated,  B.fixed=mod6_1$B.fixed.estimated )

# estimate the model with initial item parameters from mod6_1
mod6_1b <- TAM::tam.mml.2pl( resp=data.gpcm, irtmodel="2PL",
       xsi.inits=mod6_1$xsi.fixed.estimated,  B=mod6_1$B )

#************************************************************
# Ex6.2: Generalized partial credit model
mod6_2 <- TAM::tam.mml.2pl( resp=data.gpcm, irtmodel="GPCM", control=list(maxiter=200))
mod6_2$B[,2,]    # joint slope parameter for all categories

#************************************************************
# Ex6.3: some fixed entries of slope matrix B
# B: nitems x maxK x ndim
#   ( number of items x maximum number of categories x number of dimensions)
# set two constraints
B.fixed <- matrix( 0, 2, 4 )
# set second item, score of 2 (category 3), at first dimension to 2.3
B.fixed[1,] <- c(2,3,1,2.3)
# set third item, score of 1 (category 2), at first dimension to 1.4
B.fixed[2,] <- c(3,2,1,1.4)

# estimate item parameter with variance fixed (by default)
mod6_3 <- TAM::tam.mml.2pl( resp=data.gpcm, irtmodel="2PL", B.fixed=B.fixed,
                 control=list( maxiter=200) )
mod6_3$B

#************************************************************
# Ex 6.4: estimate the same model, but estimate variance
mod6_4 <- TAM::tam.mml.2pl( resp=data.gpcm, irtmodel="2PL", B.fixed=B.fixed,
               est.variance=TRUE, control=list( maxiter=350) )
mod6_4$B

#************************************************************
# Ex 6.5: partial credit model
mod6_5 <- TAM::tam.mml( resp=data.gpcm,control=list( maxiter=200) )
mod6_5$B

#************************************************************
# Ex 6.6: partial credit model: Conquest parametrization 'item+item*step'
mod6_6 <- TAM::tam.mml( resp=data.gpcm, irtmodel="PCM2" )
summary(mod6_6)

# estimate mod6_6 applying the sum constraint of item difficulties
# modify design matrix of xsi paramters
A1 <- TAM::.A.PCM2(resp=data.gpcm )
A1[3,2:4,"Comfort"] <- 1:3
A1[3,2:4,"Work"] <- 1:3
A1 <- A1[,, -3] # remove Benefit xsi item parameter
# estimate model
mod6_6b <- TAM::tam.mml( resp=data.gpcm, A=A1, beta.fixed=FALSE )
summary(mod6_6b)

# estimate model with argument constraint="items"
mod6_6c <- TAM::tam.mml( resp=data.gpcm, irtmodel="PCM2", constraint="items")

# estimate mod6_6 using tam.mml.mfr
mod6_6d <- TAM::tam.mml.mfr( resp=data.gpcm, formulaA=~ 0 + item + item:step,
    control=list(fac.oldxsi=.1), constraint="items" )
summary(mod6_6d)

#************************************************************
# Ex 6.7: Rating scale model: Conquest parametrization 'item+step'
mod6_7 <- TAM::tam.mml( resp=data.gpcm, irtmodel="RSM" )
summary(mod6_7)

#************************************************************
# Ex 6.8: sum constraint on item difficulties
#         partial credit model: ConQuest parametrization 'item+item*step'
#         polytomous scored TIMMS data
#         compare to Example 16
#

data(data.timssAusTwn.scored)
dat <- data.timssAusTwn.scored[,1:11]

## > tail(sort(names(dat)),1) # constrained item
## [1] "M032761"

# modify design matrix of xsi paramters
A1 <- TAM::.A.PCM2( resp=dat )
# constrained item loads on every other main item parameter
# with opposing margin it had been loaded on its own main item parameter
A1["M032761",,setdiff(colnames(dat), "M032761")] <- -A1["M032761",,"M032761"]
# remove main item parameter for constrained item
A1 <- A1[,, setdiff(dimnames(A1)[[3]],"M032761")]

# estimate model
mod6_8a <- TAM::tam.mml( resp=dat, A=A1, beta.fixed=FALSE )
summary(mod6_8a)
# extract fixed item parameter for item M032761
## - sum(mod6_8a$xsi[setdiff(colnames(dat), "M032761"),"xsi"])

# estimate mod6_8a using tam.mml.mfr
## fixed a bug in 'tam.mml.mfr' for differing number of categories
## per item -> now a xsi vector with parameter fixings to values
## of 99 is used
mod6_8b <- TAM::tam.mml.mfr( resp=dat, formulaA=~ 0 + item + item:step,
                        control=list(fac.oldxsi=.1), constraint="items" )
summary(mod6_8b)

#************************************************************
# Ex 6.9: sum constraint on item difficulties for irtmodel="PCM"

data(data.timssAusTwn.scored)
dat <- data.timssAusTwn.scored[,2:11]
dat[ dat==9 ] <- NA

# obtain the design matrix for the PCM parametrization and
# the number of categories for each item
maxKi <- apply(dat, 2, max, na.rm=TRUE)
des <- TAM::designMatrices(resp=dat)
A1 <- des$A

# define the constrained item category and remove the respective parameter
(par <- unlist( strsplit(dimnames(A1)[[3]][dim(A1)[3]], split="_") ))
A1 <- A1[,,-dim(A1)[3]]

# the item category loads on every other item category parameter with
# opposing margin, balancing the number of categories for each item
item.id <- which(colnames(dat)==par[1])
cat.id <- maxKi[par[1]]+1
loading <- 1/rep(maxKi, maxKi)
loading <- loading [-which(names(loading)==par[1])[1]]
A1[item.id, cat.id, ] <- loading
A1[item.id,,]

# estimate model
mod6_9 <- TAM::tam.mml( resp=dat, A=A1, beta.fixed=FALSE )
summary(mod6_9)

## extract fixed item category parameter
# calculate mean for each item
ind.item.cat.pars <- sapply(colnames(dat), grep, rownames(mod6_8$xsi))
item.means <- lapply(ind.item.cat.pars, function(ii) mean(mod6_8$xsi$xsi[ii]))

# these sum up to the negative of the fixed parameter
fix.par <- -sum( unlist(item.means), na.rm=TRUE)

#************************************************************
# Ex 6.10: Generalized partial credit model with equality constraints
#          on item discriminations

data(data.gpcm)
dat <- data.gpcm

# Ex 6.10a: set all slopes of three items equal to each other
E <- matrix( 1, nrow=3, ncol=1 )
mod6_10a <- TAM::tam.mml.2pl( dat, irtmodel="GPCM.design", E=E  )
summary(mod6_10a)
mod6_10a$B[,,]

# Ex 6.10b: equal slope for first and third item
E <- matrix( 0, nrow=3, ncol=2 )
E[c(1,3),1] <- 1
E[ 2, 2 ] <- 1
mod6_10b <- TAM::tam.mml.2pl( dat, irtmodel="GPCM.design", E=E  )
summary(mod6_10b)
mod6_10b$B[,,]

#############################################################################
# EXAMPLE 7: design matrix for slopes for the generalized partial credit model
#############################################################################

# (1) simulate data from a model with a (item slope) design matrix E
set.seed(789)
I <- 42
b <- seq( -2, 2, len=I)
# create design matrix for loadings
E <- matrix( 0, I, 5 )
E[ seq(1,I,3), 1 ] <- 1
E[ seq(2,I,3), 2 ] <- 1
E[ seq(3,I,3), 3 ] <- 1
ind <- seq( 1, I, 2 ) ; E[ ind, 4 ] <- rep( c( .3, -.2 ), I )[ 1:length(ind) ]
ind <- seq( 2, I, 4 ) ; E[ ind, 5 ] <- rep( .15, I )[ 1:length(ind) ]
E

# true basis slope parameters
lambda <- c( 1, 1.2, 0.8, 1, 1.1 )
# calculate item slopes
a <- E \%*\% lambda

# simulate
N <- 4000
theta <- stats::rnorm( N )
aM <- outer( rep(1,N), a[,1] )
bM <- outer( rep(1,N), b )
pM <- stats::plogis( aM * ( matrix( theta, nrow=N, ncol=I  ) - bM ) )
dat <- 1 * ( pM > stats::runif( N*I ) )
colnames(dat) <- paste("I", 1:I, sep="")

# estimate model
mod7 <- TAM::tam.mml.2pl( resp=dat, irtmodel="GPCM.design", E=E )
mod7$B

# recalculate estimated basis parameters
stats::lm( mod7$B[,2,1] ~ 0+ as.matrix(E ) )
  ##   Call:
  ##   lm(formula=mod7$B[, 2, 1] ~ 0 + as.matrix(E))
  ##   Coefficients:
  ##   as.matrix(E)1  as.matrix(E)2  as.matrix(E)3  as.matrix(E)4  as.matrix(E)5
  ##          0.9904         1.1896         0.7817         0.9601         1.2132

#############################################################################
# EXAMPLE 8: Differential item functioning                                  #
#  A first example of a Multifaceted Rasch Model                            #
#  Facet is only female; 10 items are studied                               #
#############################################################################
data(data.ex08)

formulaA <- ~ item+female+item*female
# this formula is in R equivalent to 'item*female'
resp <- data.ex08[["resp"]]
facets <- as.data.frame( data.ex08[["facets"]] )

#***
# Model 8a: investigate gender DIF on all items
mod8a <- TAM::tam.mml.mfr( resp=resp, facets=facets, formulaA=formulaA )
summary(mod8a)

#***
# Model 8a 2: specification with long format response data
resp.long <- c( data.ex08[["resp"]] )
pid <- rep( 1:nrow(data.ex08[["resp"]]), ncol(data.ex08[["resp"]]) )

itemnames <- rep(colnames(data.ex08[["resp"]]), each=nrow(data.ex08[["resp"]]))
facets.long <- cbind( data.frame( "item"=itemnames ),
                 data.ex08[["facets"]][pid,,drop=F] )

mod8a_2 <- TAM::tam.mml.mfr( resp=resp.long, facets=facets.long,
                      formulaA=formulaA, pid=pid)
stopifnot( all(mod8a$xsi.facets$xsi==mod8a_2$xsi.facets$xsi) )

#***
# Model 8b: Differential bundle functioning (DBF)
#   - investigate differential item functioning in item groups

# modify pre-specified design matrix to define 'appropriate' DBF effects
formulaA <- ~ item*female
des <- TAM::designMatrices.mfr( resp=resp, facets=facets, formulaA=formulaA)
A1 <- des$A$A.3d
# item group A: items 1-5
# item group B: items 6-8
# item group C: items 9-10
A1 <- A1[,,1:13]
dimnames(A1)[[3]][ c(12,13) ] <- c("A:female1", "B:female1")
# item group A
A1[,2,12] <- 0
A1[c(1,5,7,9,11),2,12] <- -1
A1[c(1,5,7,9,11)+1,2,12] <- 1
# item group B
A1[,2,13] <- 0
A1[c(13,15,17),2,13] <- -1
A1[c(13,15,17)+1,2,13] <- 1
# item group C (define effect(A)+effect(B)+effect(C)=0)
A1[c(19,3),2,c(12,13)] <- 1
A1[c(19,3)+1,2,c(12,13)] <- -1
#   A1[,2,]   # look at modified design matrix
# estimate model
mod8b <- TAM::tam.mml( resp=des$gresp$gresp.noStep, A=A1 )
summary(mod8b)

#############################################################################
# EXAMPLE 9: Multifaceted Rasch Model
#############################################################################

data(data.sim.mfr)
data(data.sim.facets)

# two way interaction item and rater
formulaA <- ~item+item:step + item*rater
mod9a <- TAM::tam.mml.mfr( resp=data.sim.mfr, facets=data.sim.facets, formulaA=formulaA)
mod9a$xsi.facets
summary(mod9a)

# three way interaction item, female and rater
formulaA <- ~item+item:step + female*rater + female*item*step
mod9b <- TAM::tam.mml.mfr( resp=data.sim.mfr, facets=data.sim.facets, formulaA=formulaA)
summary(mod9b)

#############################################################################
# EXAMPLE 10: Model with raters.
#   Persons are arranged in multiple rows which is indicated
#   by multiple person identifiers.
#############################################################################
data(data.ex10)
dat <- data.ex10
head(dat)
  ##     pid rater I0001 I0002 I0003 I0004 I0005
  ## 1     1     1     0     1     1     0     0
  ## 451   1     2     1     1     1     1     0
  ## 901   1     3     1     1     1     0     1
  ## 452   2     2     1     1     1     0     1
  ## 902   2     3     1     1     0     1     1

facets <- dat[, "rater", drop=FALSE ] # define facet (rater)
pid <- dat$pid      # define person identifier (a person occurs multiple times)
resp <- dat[, -c(1:2) ]        # item response data
formulaA <- ~ item * rater      # formula

mod10 <- TAM::tam.mml.mfr( resp=resp, facets=facets, formulaA=formulaA, pid=dat$pid )
summary(mod10)

# estimate person parameter with WLE
wmod10 <- TAM::tam.wle( mod10 )

#--- Example 10a
# compare model containing only item
formulaA <- ~ item + rater      # pseudo formula for item
xsi.setnull <- "rater"          # set all rater effects to zero
mod10a <- TAM::tam.mml.mfr( resp=resp, facets=facets, formulaA=formulaA,
            xsi.setnull=xsi.setnull, pid=dat$pid, beta.fixed=cbind(1,1,0))
summary(mod10a)

# A shorter way for specifying this example is
formulaA <- ~ item + 0*rater        # set all rater effects to zero
mod10a1 <- TAM::tam.mml.mfr( resp=resp, facets=facets, formulaA=formulaA, pid=dat$pid )
summary(mod10a1)

# tam.mml.mfr also appropriately extends the facets data frame with pseudo facets
# if necessary
formulaA <- ~ item     # omitting the rater term
mod10a2 <- TAM::tam.mml.mfr( resp=resp, facets=facets, formulaA=formulaA, pid=dat$pid )
  ##   Item Parameters Xsi
  ##              xsi se.xsi
  ##   I0001   -1.931  0.111
  ##   I0002   -1.023  0.095
  ##   I0003   -0.089  0.089
  ##   I0004    1.015  0.094
  ##   I0005    1.918  0.110
  ##   psfPF11  0.000  0.000
  ##   psfPF12  0.000  0.000

#***
# Model 10_2: specification with long format response data
resp.long <- c(unlist( dat[, -c(1:2) ] ))

pid <- rep( dat$pid, ncol(dat[, -c(1:2) ]) )
itemnames <- rep(colnames(dat[, -c(1:2) ]), each=nrow(dat[, -c(1:2) ]))

# quick note: the 'trick' to use pid as the row index of the facet  (cf., used in Ex 8a_2)
# is not working here, since pid already occures multiple times in the original response data
facets <- cbind( data.frame("item"=itemnames),
                 dat[ rep(1:nrow(dat), ncol(dat[,-c(1:2)])), "rater",drop=F]
)

mod10_2 <- TAM::tam.mml.mfr( resp=resp.long, facets=facets, formulaA=formulaA, pid=pid)

stopifnot( all(mod10$xsi.facets$xsi==mod10_2$xsi.facets$xsi) )

#############################################################################
# EXAMPLE 11: Dichotomous data with missing and omitted responses
#############################################################################
data(data.ex11) ; dat <- data.ex11

#***
# Model 11a: Calibration (item parameter estimating) in which omitted
#            responses (code 9) are set to missing
dat1 <- dat[,-1]
dat1[ dat1==9 ] <- NA
# estimate Rasch model
mod11a <- TAM::tam.mml( resp=dat1 )
summary(mod11a)
# compute person parameters
wmod11a <- TAM::tam.wle( mod11a )

#***
# Model 11b: Scaling persons (WLE estimation) setting omitted
#            responses as incorrect and using fixed
#            item parameters form Model 11a

# set matrix with fixed item difficulties as the input
xsi1 <- mod11a$xsi    # xsi output from Model 11a
xsi.fixed <- cbind( seq(1,nrow(xsi1) ), xsi1$xsi )
# recode 9 to 0
dat2 <- dat[,-1]
dat2[ dat2==9 ] <- 0
# run Rasch model with fixed item difficulties
mod11b <- TAM::tam.mml( resp=dat2, xsi.fixed=xsi.fixed )
summary(mod11b)
# WLE estimation
wmod11b <- TAM::tam.wle( mod11b )

#############################################################################
# EXAMPLE 12: Avoiding nonconvergence using the argument increment.factor
#############################################################################
data(data.ex12)
dat <- data.ex12

# non-convergence without increment.factor
mod1 <- TAM::tam.mml.2pl( resp=data.ex12, control=list( maxiter=1000) )

# avoiding non-convergence with increment.factor=1.02
mod2 <- TAM::tam.mml.2pl( resp=data.ex12,
            control=list( maxiter=1000, increment.factor=1.02) )
summary(mod1)
summary(mod2)

#############################################################################
# EXAMPLE 13: Longitudinal data 'data.long' from sirt package
#############################################################################
library(sirt)
data(data.long, package="sirt")
dat <- data.long
  ##   > colnames(dat)
  ##    [1] "idstud" "I1T1"   "I2T1"   "I3T1"   "I4T1"   "I5T1"   "I6T1"
  ##    [8] "I3T2"   "I4T2"   "I5T2"   "I6T2"   "I7T2"   "I8T2"

## item 1 to 6 administered at T1 and items 3 to 8 at T2
## items 3 to 6 are anchor items

#***
# Model 13a: 2-dimensional Rasch model assuming invariant item difficulties

# define matrix loadings
Q <- matrix(0,12,2)
colnames(Q) <- c("T1","T2")
Q[1:6,1] <- 1       # items at T1
Q[7:12,2] <- 1      # items at T2

# assume equal item difficulty of I3T1 and I3T2, I4T1 and I4T2, ...
# create draft design matrix and modify it
A <- TAM::designMatrices(resp=data.long[,-1])$A
dimnames(A)[[1]] <- colnames(data.long)[-1]
  ##   > str(A)
  ##    num [1:12, 1:2, 1:12] 0 0 0 0 0 0 0 0 0 0 ...
  ##    - attr(*, "dimnames")=List of 3
  ##     ..$ : chr [1:12] "Item01" "Item02" "Item03" "Item04" ...
  ##     ..$ : chr [1:2] "Category0" "Category1"
  ##     ..$ : chr [1:12] "I1T1" "I2T1" "I3T1" "I4T1" ...
A1 <- A[,, c(1:6, 11:12 ) ]
dimnames(A1)[[3]] <- substring( dimnames(A1)[[3]],1,2)
A1[7,2,3] <- -1     # difficulty(I3T1)=difficulty(I3T2)
A1[8,2,4] <- -1     # I4T1=I4T2
A1[9,2,5] <- A1[10,2,6] <- -1
  ##   > A1[,2,]
  ##        I1 I2 I3 I4 I5 I6 I7 I8
  ##   I1T1 -1  0  0  0  0  0  0  0
  ##   I2T1  0 -1  0  0  0  0  0  0
  ##   I3T1  0  0 -1  0  0  0  0  0
  ##   I4T1  0  0  0 -1  0  0  0  0
  ##   I5T1  0  0  0  0 -1  0  0  0
  ##   I6T1  0  0  0  0  0 -1  0  0
  ##   I3T2  0  0 -1  0  0  0  0  0
  ##   I4T2  0  0  0 -1  0  0  0  0
  ##   I5T2  0  0  0  0 -1  0  0  0
  ##   I6T2  0  0  0  0  0 -1  0  0
  ##   I7T2  0  0  0  0  0  0 -1  0
  ##   I8T2  0  0  0  0  0  0  0 -1

# estimate model
# set intercept of second dimension (T2) to zero
beta.fixed <- cbind( 1, 2, 0 )
mod13a <- TAM::tam.mml( resp=data.long[,-1], Q=Q, A=A1, beta.fixed=beta.fixed)
summary(mod13a)

#--- tamaan specification
tammodel <- "
  LAVAAN MODEL:
    T1=~ 1*I1T1__I6T1
    T2=~ 1*I3T2__I8T2
    T1 ~~ T1
    T2 ~~ T2
    T1 ~~ T2
    # constraint on item difficulties
    I3T1 + I3T2 | b3*t1
    I4T1 + I4T2 | b4*t1
    I5T1 + I5T2 | b5*t1
    I6T1 + I6T2 | b6*t1
    "
# The constraint on item difficulties can be more efficiently written as
  ##       DO(3,6,1)
  ##         I\%T1 + I\%T2 | b\%*t1
  ##       DOEND
# estimate model
mod13at <- TAM::tamaan( tammodel, resp=data.long,  beta.fixed=beta.fixed )
summary(mod13at)

#***
# Model 13b: invariant item difficulties with zero mean item difficulty
#           of anchor items

A <- TAM::designMatrices(resp=data.long[,-1])$A
dimnames(A)[[1]] <- colnames(data.long)[-1]
A1 <- A[,, c(1:5, 11:12 ) ]
dimnames(A1)[[3]] <- substring( dimnames(A1)[[3]],1,2)
A1[7,2,3] <- -1     # difficulty(I3T1)=difficulty(I3T2)
A1[8,2,4] <- -1     # I4T1=I4T2
A1[9,2,5] <- -1
A1[6,2,3] <- A1[6,2,4] <- A1[6,2,5] <- 1     # I6T1=-(I3T1+I4T1+I5T1)
A1[10,2,3] <- A1[10,2,4] <- A1[10,2,5] <- 1  # I6T2=-(I3T2+I4T2+I5T2)
A1[,2,]
  ##      I1 I2 I3 I4 I5 I7 I8
  ## I1T1 -1  0  0  0  0  0  0
  ## I2T1  0 -1  0  0  0  0  0
  ## I3T1  0  0 -1  0  0  0  0
  ## I4T1  0  0  0 -1  0  0  0
  ## I5T1  0  0  0  0 -1  0  0
  ## I6T1  0  0  1  1  1  0  0
  ## I3T2  0  0 -1  0  0  0  0
  ## I4T2  0  0  0 -1  0  0  0
  ## I5T2  0  0  0  0 -1  0  0
  ## I6T2  0  0  1  1  1  0  0
  ## I7T2  0  0  0  0  0 -1  0
  ## I8T2  0  0  0  0  0  0 -1

mod13b <- TAM::tam.mml( resp=data.long[,-1], Q=Q, A=A1, beta.fixed=FALSE)
summary(mod13b)

#***
# Model 13c: longitudinal polytomous data
#

# modifiy Items I1T1, I4T1, I4T2 in order to be trichotomous (codes: 0,1,2)
set.seed(42)
dat <- data.long
dat[(1:50),2] <- sample(c(0,1,2), 50, replace=TRUE)
dat[(1:50),5] <- sample(c(0,1,2), 50, replace=TRUE)
dat[(1:50),9] <- sample(c(0,1,2), 50, replace=TRUE)
  ##   > colnames(dat)
  ##    [1] "idstud" "I1T1"   "I2T1"   "I3T1"   "I4T1"   "I5T1"   "I6T1"
  ##    [8] "I3T2"   "I4T2"   "I5T2"   "I6T2"   "I7T2"   "I8T2"

## item 1 to 6 administered at T1, items 3 to 8 at T2
## items 3 to 6 are anchor items

# (1) define matrix loadings
Q <- matrix(0,12,2)
colnames(Q) <- c("T1","T2")
Q[1:6,1] <- 1       # items at T1
Q[7:12,2] <- 1      # items at T2

# (2) assume equal item difficulty of anchor items
#     create draft design matrix and modify it
A <- TAM::designMatrices(resp=dat[,-1])$A
dimnames(A)[[1]] <- colnames(dat)[-1]
  ## > str(A)
  ## num [1:12, 1:3, 1:15] 0 0 0 0 0 0 0 0 0 0 ...
  ## - attr(*, "dimnames")=List of 3
  ## ..$ : chr [1:12] "I1T1" "I2T1" "I3T1" "I4T1" ...
  ## ..$ : chr [1:3] "Category0" "Category1" "Category2"
  ## ..$ : chr [1:15] "I1T1_Cat1" "I1T1_Cat2" "I2T1_Cat1" "I3T1_Cat1" ...

# define matrix A
# Items 1 to 3 administered at T1, Items 3 to 6 are anchor items
# Item 7 to 8 administered at T2
# Item I1T1, I4T1, I4T2 are trichotomous (codes: 0,1,2)
A1 <- A[,, c(1:8, 14:15) ]
dimnames(A1)[[3]] <- gsub("T1|T2", "",  dimnames(A1)[[3]])

# Modifications are shortened compared to Model 13 a, but are still valid
A1[7,,] <- A1[3,,]  # item 7, i.e. I3T2, loads on same parameters as
                    # item 3, I3T1
A1[8,,] <- A1[4,,]  # same for item 8 and item 4
A1[9,,] <- A1[5,,]  # same for item 9 and item 5
A1[10,,] <- A1[6,,] # same for item 10 and item 6
  ## > A1[8,,]
  ##           I1_Cat1 I1_Cat2 I2_Cat1 I3_Cat1 I4_Cat1 I4_Cat2 I5_Cat1 ...
  ## Category0       0       0       0       0       0       0       0
  ## Category1       0       0       0       0      -1       0       0
  ## Category2       0       0       0       0      -1      -1       0

# (3) estimate model
#     set intercept of second dimension (T2) to zero
beta.fixed <- cbind( 1, 2, 0 )
mod13c <- TAM::tam.mml( resp=dat[,-1], Q=Q, A=A1, beta.fixed=beta.fixed,
                   irtmodel="PCM")
summary(mod13c)
wle.mod13c <- TAM::tam.wle(mod13c) # WLEs of dimension T1 and T2

#############################################################################
# EXAMPLE 14: Facet model with latent regression
#############################################################################
data( data.ex14 )
dat <- data.ex14

#***
# Model 14a: facet model
resp <- dat[, paste0("crit",1:7,sep="") ]    # item data
facets <- data.frame( "rater"=dat$rater )     # define facets
formulaA <- ~item+item*step + rater
mod14a <- TAM::tam.mml.mfr( resp, facets=facets, formulaA=formulaA, pid=dat$pid )
summary(mod14a)

#***
# Model 14b: facet model with latent regression
#   Note that dataY must correspond to rows in resp and facets which means
#   that there must be the same rows in Y for a person with multiple rows
#   in resp
dataY <- dat[, c("X1","X2") ]        # latent regressors
formulaY <- ~ X1+X2            # latent regression formula
mod14b <- TAM::tam.mml.mfr( resp, facets=facets, formulaA=formulaA,
            dataY=dataY, formulaY=formulaY, pid=dat$pid)
summary(mod14b)

#***
# Model 14c: Multi-facet model with item slope estimation
# use design matrix and modified response data from Model 1
# item-specific slopes

resp1 <- mod14a$resp      # extract response data with generalized items
A <- mod14a$A             # extract design matrix for item intercepts
colnames(resp1)

# define design matrix for slopes
E <- matrix( 0, nrow=ncol(resp1), ncol=7 )
colnames(E) <- paste0("crit",1:7)
rownames(E) <- colnames(resp1)
E[ cbind( 1:(7*7), rep(1:7,each=7) ) ] <- 1

mod14c <- TAM::tam.mml.2pl( resp=resp1, A=A, irtmodel="GPCM.design", E=E,
        control=list(maxiter=100) )
summary(mod14c)

#############################################################################
# EXAMPLE 15: Coping with nonconvergent models
#############################################################################

data(data.ex15)
data <- data.ex15
# facet model 'group*item' is of interest

#***
# Model 15a:
mod15a <- TAM::tam.mml.mfr(resp=data[,-c(1:2)],facets=data[,"group",drop=FALSE],
    formulaA=~ item + group*item, pid=data$pid )
# See output:
  ##
  ##   Iteration 47     2013-09-10 16:51:39
  ##   E Step
  ##   M Step Intercepts   |----
  ##     Deviance=75510.2868 | Deviance change: -595.0609
  ##   !!! Deviance increases!                                        !!!!
  ##   !!! Choose maybe fac.oldxsi > 0 and/or increment.factor > 1    !!!!
  ##     Maximum intercept parameter change: 0.925045
  ##     Maximum regression parameter change: 0
  ##     Variance:  0.9796  | Maximum change: 0.009226

#***
# Model 15b: Follow the suggestions of changing the default of fac.oldxsi and
#            increment.factor
mod15b <- TAM::tam.mml.mfr(resp=data[,-c(1:2)],facets=data[,"group",drop=FALSE],
            formulaA=~ group*item, pid=data$pid,
            control=list( increment.factor=1.03, fac.oldxsi=.4 ) )

#***
# Model 15c: Alternatively, just choose more iterations in M-step by "Msteps=10"
mod15c <- TAM::tam.mml.mfr(resp=data[,-c(1:2)],facets=data[,"group",drop=FALSE],
    formulaA=~ item + group*item, pid=data$pid,
    control=list(maxiter=250, Msteps=10))

#############################################################################
# EXAMPLE 16: Differential item function for polytomous items and
#             differing number of response options per item
#############################################################################

data(data.timssAusTwn.scored)
dat <- data.timssAusTwn.scored
# extract item response data
resp <- dat[, sort(grep("M", colnames(data.timssAusTwn.scored), value=TRUE)) ]
# some descriptives
psych::describe(resp)
# define facets: 'cnt' is group identifier
facets <- data.frame( "cnt"=dat$IDCNTRY)
# create design matrices
des2 <- TAM::designMatrices.mfr2( resp=resp, facets=facets,
                   formulaA=~item*step + item*cnt)
# restructured data set: pseudoitem=item x country
resp2 <- des2$gresp$gresp.noStep
# A design matrix
A <- des2$A$A.3d
    # redundant xsi parameters must be eliminated from design matrix
xsi.elim <- des2$xsi.elim
A <- A[,, - xsi.elim[,2] ]
# extract loading matrix B
B <- des2$B$B.3d
# estimate model
mod1 <- TAM::tam.mml( resp=resp2, A=A, B=B, control=list(maxiter=100) )
summary(mod1)
# The sum of all DIF parameters is set to zero. The DIF parameter for the last
# item is therefore obtained
xsi1 <- mod1$xsi
difxsi <- xsi1[ intersect( grep("cnt",rownames(xsi1)),
              grep("M03",rownames(xsi1))), ]   - colSums(difxsi)
    # this is the DIF effect of the remaining item

#############################################################################
# EXAMPLE 17: Several multidimensional and subdimension models
#############################################################################

library(mirt)
#*** (1) simulate data in mirt package
set.seed(9897)
# simulate data according to the four-dimensional Rasch model
# variances
variances <- c( 1.45, 1.74, .86, 1.48  )
# correlations
corrs <- matrix( 1, 4, 4 )
dd1 <- 1 ; dd2 <- 2 ; corrs[dd1,dd2] <- corrs[dd2,dd1] <- .88
dd1 <- 1 ; dd2 <- 3 ; corrs[dd1,dd2] <- corrs[dd2,dd1] <- .85
dd1 <- 1 ; dd2 <- 4 ; corrs[dd1,dd2] <- corrs[dd2,dd1] <- .87
dd1 <- 2 ; dd2 <- 3 ; corrs[dd1,dd2] <- corrs[dd2,dd1] <- .84
dd1 <- 2 ; dd2 <- 4 ; corrs[dd1,dd2] <- corrs[dd2,dd1] <- .90
dd1 <- 3 ; dd2 <- 4 ; corrs[dd1,dd2] <- corrs[dd2,dd1] <- .90
# covariance matrix
covar <- outer( sqrt( variances), sqrt(variances) )*corrs
# item thresholds and item discriminations
d <- matrix( stats::runif(40, -2, 2 ), ncol=1 )
a <- matrix(NA, nrow=40,ncol=4)
a[1:10,1] <- a[11:20,2] <- a[21:30,3] <- a[31:40,4] <- 1
# simulate data
dat <- mirt::simdata(a=a, d=d, N=1000, itemtype="dich", sigma=covar )
# define Q-matrix for testlet and subdimension models estimated below
Q <- matrix( 0, nrow=40, ncol=5 )
colnames(Q) <- c("g", paste0( "subd", 1:4) )
Q[,1] <- 1
Q[1:10,2] <- Q[11:20,3] <- Q[21:30,4] <- Q[31:40,5] <- 1

# define maximum number of iterations and number of quasi monte carlo nodes
# maxit <- 5  ; snodes <- 300    # this specification is only for speed reasons
maxit <- 200 ; snodes <- 1500

#*****************
# Model 1: Rasch testlet model
#*****************

# define a user function for restricting the variance according to the
# Rasch testlet model
variance.fct1 <- function( variance ){
            ndim <- ncol(variance)
            variance.new <- matrix( 0, ndim, ndim )
            diag(variance.new) <- diag(variance)
            variance <- variance.new
            return(variance)
                    }
variance.Npars <- 5    # number of estimated parameters in variance matrix
# estimation using tam.mml
mod1 <- TAM::tam.mml( dat, Q=Q, userfct.variance=variance.fct1,
             variance.Npars=variance.Npars, control=list(maxiter=maxit, QMC=TRUE,
                          snodes=snodes))
summary(mod1)

#*****************
# Model 2: Testlet model with correlated testlet effects
#*****************

# specify a testlet model with general factor g and testlet effects
# u_1,u_2,u_3 and u_4. Assume that Cov(g,u_t)=0 for all t=1,2,3,4.
# Additionally, assume that \sum_t,t' Cov( u_t, u_t')=0, i.e.
# the sum of all testlet covariances is equal to zero
#=> testlet effects are uncorrelated on average.

# set Cov(g,u_t)=0 and sum of all testlet covariances equals to zero
variance.fct2 <- function( variance ){
            ndim <- ncol(variance)
            variance.new <- matrix( 0, ndim, ndim )
            diag(variance.new) <- diag(variance)
            variance.new[1,2:ndim] <- variance.new[2:ndim,1] <- 0
            # calculate average covariance between testlets
            v1 <- variance[ -1, -1] - variance.new[-1,-1]
            M1 <- sum(v1) / ( ( ndim-1)^2 - ( ndim - 1))
            v1 <- v1 - M1
            variance.new[ -1, -1 ] <- v1
            diag(variance.new) <- diag(variance)
            variance <- variance.new
            return(variance)
                    }
variance.Npars <- 1 + 4 + (4*3)/2 - 1
# estimate model in TAM
mod2 <- TAM::tam.mml( dat, Q=Q, userfct.variance=variance.fct2,
                variance.Npars=variance.Npars,
                control=list(maxiter=maxit, QMC=TRUE, snodes=snodes) )
summary(mod2)

#*****************
# Model 3: Testlet model with correlated testlet effects (different identification)
#*****************

# Testlet model like in Model 2. But now the constraint is
# \sum _t,t' Cov(u_t, u_t') + \sum_t Var(u_t)=0, i.e.
# the sum of all testlet covariances and variances is equal to zero.
variance.fct3 <- function( variance ){
            ndim <- ncol(variance)
            variance.new <- matrix( 0, ndim, ndim )
            diag(variance.new) <- diag(variance)
            variance.new[1,2:ndim] <- variance.new[2:ndim,1] <- 0
            # calculate average covariance and variance between testlets
            v1 <- variance[ -1, -1]
            M1 <- mean(v1)
            v1 <- v1 - M1
            variance.new[ -1, -1 ] <- v1
            # ensure positive definiteness of covariance matrix
            eps <- 10^(-2)
            diag(variance.new) <- diag( variance.new) + eps
            variance.new <- psych::cor.smooth( variance.new )  # smoothing in psych
            variance <- variance.new
            return(variance)
                    }
variance.Npars <- 1 + 4 + (4*3)/2 - 1
# estimate model in TAM
mod3 <- TAM::tam.mml( dat, Q=Q, userfct.variance=variance.fct3,
                variance.Npars=variance.Npars,
                control=list(maxiter=maxit, QMC=TRUE, snodes=snodes) )
summary(mod3)

#*****************
# Model 4: Rasch subdimension model
#*****************

# The Rasch subdimension model is specified according to Brandt (2008).
# The fourth testlet effect is defined as u4=- (u1+u2+u3)
# specify an alternative Q-matrix with 4 dimensions
Q2 <- Q[,-5]
Q2[31:40,2:4] <- -1

# set Cov(g,u1)=Cov(g,u2)=Cov(g,u3)=0
variance.fixed <- rbind( c(1,2,0), c(1,3,0), c(1,4,0) )
# estimate model in TAM
mod4 <- TAM::tam.mml( dat, Q=Q2,variance.fixed=variance.fixed,
                control=list(maxiter=maxit, QMC=TRUE, snodes=snodes) )
summary(mod4)

#*****************
# Model 5: Higher-order model
#*****************

# A four-dimensional model with a higher-order factor is specified.
# F_t=a_t g + eps_g
Q3 <- Q[,-1]
# define fitting function using the lavaan package and ULS estimation
N0 <- nrow(dat)         # sample size of dataset
library(lavaan)        # requires lavaan package for fitting covariance
variance.fct5 <- function( variance ){
    ndim <- ncol(variance)
    rownames(variance) <- colnames(variance) <- paste0("F",1:ndim)
    lavmodel <- paste0(
        "FHO=~", paste0( paste0( "F", 1:ndim ), collapse="+" ) )
    lavres <- lavaan::cfa( model=lavmodel, sample.cov=variance, estimator="ULS",
                       std.lv=TRUE, sample.nobs=N0)
    variance.new <- fitted(lavres)$cov
    variance <- variance.new
    # print coefficients
    cat( paste0( "\n **** Higher order loadings: ",
            paste0( paste0( round( coef(lavres)[ 1:ndim ], 3 )), collapse=" ")
                        ), "\n")
    return(variance)
                    }
variance.Npars <- 4+4
# estimate model in TAM
mod5 <- TAM::tam.mml( dat, Q=Q3, userfct.variance=variance.fct5,
                variance.Npars=variance.Npars,
                control=list(maxiter=maxit, QMC=TRUE, snodes=snodes) )
summary(mod5)

#*****************
# Model 6: Generalized Rasch subdimension model (Brandt, 2012)
#*****************

Q2 <- Q[,-5]
Q2[31:40,2:4] <- -1
# fixed covariances
variance.fixed2 <- rbind( c(1,2,0), c(1,3,0), c(1,4,0)  )
# design matrix for item loading parameters
#      items x category x dimension x xsi parameter
E <- array( 0, dim=c( 40, 2, 4, 4 ) )
E[ 1:10, 2, c(1,2), 1 ] <- 1
E[ 11:20, 2, c(1,3), 2 ] <- 1
E[ 21:30, 2, c(1,4), 3 ] <- 1
E[ 31:40, 2, 1, 4 ] <- 1
E[ 31:40, 2, 2:4, 4 ] <- -1

# constraint on slope parameters, see Brandt (2012)
gammaconstr <- function( gammaslope ){
        K <- length( gammaslope)
        g1 <- sum( gammaslope^2  )
        gammaslope.new <- sqrt(K) / sqrt(g1) * gammaslope
        return(gammaslope.new)
                    }
# estimate model
mod6 <- TAM::tam.mml.3pl( dat, E=E, Q=Q2, variance.fixed=variance.fixed2,
           skillspace="normal", userfct.gammaslope=gammaconstr, gammaslope.constr.Npars=1,
           control=list(maxiter=maxit, QMC=TRUE, snodes=snodes ) )
summary(mod6)

#############################################################################
# EXAMPLE 18: Partial credit model with dimension-specific sum constraints
#             on item difficulties
#############################################################################

data(data.Students, package="CDM")
dat <- data.Students[, c( paste0("sc",1:4), paste0("mj",1:4) ) ]
# specify dimensions in Q-matrix
Q <- matrix( 0,  nrow=8, ncol=2 )
Q[1:4,1] <- Q[5:8,2] <- 1
# partial credit model with some constraint on item parameters
mod1 <- TAM::tam.mml( dat, Q=Q, irtmodel="PCM2", constraint="items")
summary(mod1)

#############################################################################
# EXAMPLE 19: Partial credit scoring: 0.5 points for partial credit items
#             and 1 point for dichotomous items
#############################################################################

data(data.timssAusTwn.scored)
dat <- data.timssAusTwn.scored
# extrcat item response data
dat <- dat[, grep("M03", colnames(dat) ) ]

# select items with do have maximum score of 2 (polytomous items)
ind <- which( apply( dat,  2, max, na.rm=TRUE )==2 )
I <- ncol(dat)
# define Q-matrix with scoring variant
Q <- matrix( 1, nrow=I, ncol=1 )
Q[ ind, 1 ] <- .5    # score of 0.5 for polyomous items

# estimate model
mod1 <- TAM::tam.mml( dat, Q=Q, irtmodel="PCM2", control=list(nodes=seq(-10,10,len=21)))
summary(mod1)

#############################################################################
# EXAMPLE 20: Specification of loading matrix in multidimensional model
#############################################################################

data(data.gpcm)
psych::describe(data.gpcm)
resp <- data.gpcm

# define three dimensions and different loadings of item categories
# on these dimensions in B loading matrix
I <- 3  # 3 items
D <- 3  # 3 dimensions
# define loading matrix B
# 4 categories for each item (0,1,2,3)
B <- array( 0, dim=c(I,4,D) )
for (ii in 1:I){
    B[ ii, 1:4, 1 ] <- 0:3
    B[ ii, 1,2 ] <- 1
    B[ ii, 4,3 ] <- 1
            }
dimnames(B)[[1]] <- colnames(resp)
B[1,,]
  ##   > B[1,,]
  ##        [,1] [,2] [,3]
  ##   [1,]    0    1    0
  ##   [2,]    1    0    0
  ##   [3,]    2    0    0
  ##   [4,]    3    0    1
#-- test run
mod1 <- TAM::tam.mml( resp, B=B, control=list( snodes=1000, maxiter=5)  )
summary(mod1)

# Same model with TAM::tam.mml.3pl instead

dim4 <- sum(apply(B, c(1, 3), function(x) any(!(x==0))))
E1 <- array(0, dim=c(dim(B), dim4))

kkk <- 0
for (iii in seq_len(dim(E1)[1])) {
    for (jjj in seq_len(dim(E1)[3])) {
        if (any(B[iii,, jjj] !=0)) {
            kkk <- kkk + 1
            E1[iii,, jjj, kkk] <- B[iii,, jjj]
        }
    }
}
if (kkk !=dim4) stop("Something went wrong in the loop, because 'kkk !=dim4'.")

mod2 <- TAM::tam.mml.3pl(resp, E=E1, est.some.slopes=FALSE, control=list(maxiter=50))
summary(mod2)

cor(mod1$person$EAP.Dim3, mod2$person$EAP.Dim3)
cor(mod1$person$EAP.Dim2, mod2$person$EAP.Dim2)
cor(mod1$person$EAP.Dim1, mod2$person$EAP.Dim1)
cor(mod1$xsi$xsi, mod2$xsi$xsi)

#############################################################################
# EXAMPLE 21: Acceleration of EM algorithm | dichotomous data
#############################################################################

N <- 1000      # number of persons
I <- 100       # number of items
set.seed(987)
# simulate data according to the Rasch model
dat <- sirt::sim.raschtype( stats::rnorm(N), b=seq(-2,2,len=I)  )
# estimate models
mod1n <- TAM::tam.mml( resp=dat, control=list( acceleration="none") )  # no acceler.
mod1y <- TAM::tam.mml( resp=dat, control=list( acceleration="Yu") )  # Yu acceler.
mod1r <- TAM::tam.mml( resp=dat, control=list( acceleration="Ramsay") )  # Ramsay acceler.
# compare number of iterations
mod1n$iter ; mod1y$iter ; mod1r$iter
# log-likelihood values
logLik(mod1n); logLik(mod1y) ; logLik(mod1r)

#############################################################################
# EXAMPLE 22: Acceleration of EM algorithm | polytomous data
#############################################################################

data(data.gpcm)
dat <- data.gpcm

# no acceleration
mod1n <- TAM::tam.mml.2pl( resp=dat, irtmodel="GPCM",
                control=list( conv=1E-4, acceleration="none") )
# Yu acceleration
mod1y <- TAM::tam.mml.2pl( resp=dat, irtmodel="GPCM",
                control=list( conv=1E-4, acceleration="Yu") )
# Ramsay acceleration
mod1r <- TAM::tam.mml.2pl( resp=dat, irtmodel="GPCM",
                control=list( conv=1E-4, acceleration="Ramsay") )
# number of iterations
mod1n$iter ; mod1y$iter ; mod1r$iter

#############################################################################
# EXAMPLE 23: Multidimensional polytomous Rasch model in which
#             dimensions are defined by categories
#############################################################################

data(data.Students, package="CDM")
dat <- data.Students[, grep( "act", colnames(data.Students) ) ]

# define multidimensional model in which categories of item define dimensions

# * Category 0 -> loading of one on Dimension 0
# * Category 1 -> no loadings
# * Category 2 -> loading of one on Dimension 2

# extract default design matrices
res <- TAM::designMatrices( resp=dat )
A <- res$A
B0 <- 0*res$B
# create design matrix B
B <- array( 0, dim=c( dim(B0)[c(1,2) ], 2  ) )
dimnames(B)[[1]] <- dimnames(B0)[[1]]
dimnames(B)[[2]] <- dimnames(B0)[[2]]
dimnames(B)[[3]] <- c( "Dim0", "Dim2" )
B[,1,1]  <- 1
B[,3,2]  <- 1

# estimate multdimensional Rasch model
mod1 <- TAM::tam.mml( resp=dat, A=A, B=B, control=list( maxiter=100) )
summary(mod1)

# alternative definition of B
# * Category 1: negative loading on Dimension 1 and Dimension 2
B <- array( 0, dim=c( dim(B0)[c(1,2) ], 2  ) )
dimnames(B)[[1]] <- dimnames(B0)[[1]]
dimnames(B)[[2]] <- dimnames(B0)[[2]]
dimnames(B)[[3]] <- c( "Dim0", "Dim2" )
B[,1, 1]  <- 1
B[,3, 2]  <- 1
B[,2, c(1,2)]  <- -1

# estimate model
mod2 <- TAM::tam.mml( resp=dat, A=A, B=B, control=list( maxiter=100) )
summary(mod2)

#############################################################################
# EXAMPLE 24: Sum constraint on item-category parameters in partial credit model
#############################################################################

data(data.gpcm,package="TAM")
dat <- data.gpcm

# check number of categories
c1 <- TAM::tam.ctt3(dat)

#--- fit with PCM
mod1 <- TAM::tam.mml( dat )
summary(mod1, file="mod1")

#--- fit with constraint on sum of categories
#** redefine design matrix
A1 <- 0*mod1$A
A1 <- A1[,, - dim(A1)[[3]]]
str(A1)
NP <- dim(A1)[[3]]
# define item category parameters
A1[1,2,1] <- A1[1,3,2] <- A1[1,4,3] <- -1
A1[2,2,4] <- A1[2,3,5] <- A1[2,4,6] <- -1
A1[3,2,7] <- A1[3,3,8] <- -1
A1[3,4,1:8] <- 1
# check definition
A1[1,,]
A1[2,,]
A1[3,,]

#** estimate model
mod2 <- TAM::tam.mml( dat, A=A1, beta.fixed=FALSE)
summary(mod2, file="mod2")

#--- compare model fit
IRT.compareModels(mod1, mod2 )  # -> equivalent model fit

#############################################################################
# EXAMPLE 25: Different GPCM parametrizations in IRT packages
#############################################################################

library(TAM)
library(mirt)
library(ltm)

data(data.gpcm, package="TAM")
dat <- data.gpcm

#*** TAM
mod1 <- TAM::tam.mml.2pl(dat, irtmodel="GPCM")
#*** mirt
mod2 <- mirt::mirt(dat, 1, itemtype="gpcm", verbose=TRUE)
#*** ltm
mod3 <- ltm::gpcm( dat, control=list(verbose=TRUE) )
mod3b <- ltm::gpcm( dat, control=list(verbose=TRUE), IRT.param=FALSE)

#-- comparison log likelihood
logLik(mod1)
logLik(mod2)
logLik(mod3)
logLik(mod3b)

#*** intercept parametrization (like in TAM)

# TAM
mod1$B[,2,1]   # slope
mod1$AXsi      # intercepts
# mirt
coef(mod2)
# ltm
coef(mod3b, IRT.param=FALSE)[, c(4,1:3)]

#*** IRT parametrization

# TAM
mod1$AXsi / mod1$B[,2,1]
# mirt
coef(mod2, IRTpars=TRUE)
# ltm
coef(mod3)[, c(4,1:3)]

#############################################################################
# EXAMPLE 26: Differential item functioning in multdimensional models
#############################################################################

data(data.ex08, package="TAM")
formulaA <- ~ item+female+item*female
resp <- data.ex08[["resp"]]
facets <- as.data.frame(data.ex08[["facets"]])

#***  Model 8a: investigate gender DIF in undimensional model
mod8a <- TAM::tam.mml.mfr(resp=resp, facets=facets, formulaA=formulaA)
summary(mod8a)

#*** multidimensional 2PL Model
I <- 10
Q <- array(0, dim=c(I, 3))
Q[cbind(1:I, c(rep(1, 3), rep(2, 3), rep(3, 4)))] <- 1
rownames(Q) <- colnames(resp)
mod3dim2pl <- TAM::tam.mml.2pl(resp=resp, Q=Q, irtmodel="2PL",
                          control=list(snodes=2000))

#*** Combine both approaches
thisRows <- gsub("-.*", "", colnames(mod8a$resp)) #select item names

#*** uniform DIF (note irtmodel="2PL.groups" & est.slopegroups)
mod3dim2pl_udiff <- TAM::tam.mml.2pl(resp=mod8a$resp, A=mod8a$A, Q=Q[thisRows, ],
                               irtmodel="2PL.groups",
                               est.slopegroups=as.numeric(as.factor(thisRows)),
                               control=list(snodes=2000))

#*** non-uniform DIF (?); different slope parameters for each item administered to each group
mod3dim2pl_nudiff <- TAM::tam.mml.2pl(resp=mod8a$resp, A=mod8a$A, Q=Q[thisRows, ],
                                irtmodel="2PL", control=list(snodes=2000))

#*** check results
print(mod8a$xsi)
print(mod3dim2pl_udiff$xsi)
summary(mod3dim2pl_nudiff)

#*** within item dimensionality (one item loads on two dimensions)
Q2 <- Q
Q2[4,1] <- 1

# uniform DIF (note irtmodel="2PL.groups" & est.slopegroups)
mod3dim2pl_udiff2 <- TAM::tam.mml.2pl(resp=mod8a$resp, A=mod8a$A, Q=Q2[thisRows, ],
                                irtmodel="2PL.groups",
                                est.slopegroups=as.numeric(as.factor(thisRows)),
                                control=list(snodes=2000))
print(mod8a$xsi)
print(mod3dim2pl_udiff2$xsi)
print(mod3dim2pl_udiff2$xsi)

#############################################################################
# EXAMPLE 27: IRT parameterization for generalized partial credit model (GPCM) in TAM
#############################################################################

#--- read item parameters
pars <- as.numeric(miceadds::scan.vec(
"0.19029 1.25309 0.51737 -1.77046 0.94803
  0.19407 1.22680 0.34986 -1.57666 1.29726
  -0.00888 1.07093 0.31662 -1.38755 1.14809
  -0.33810 1.08205 0.48490 -1.56696 0.79547
  -0.18866 0.99587 0.37880 -1.37468 0.81114" ))
pars <- matrix( pars, nrow=5, byrow=TRUE)
beta <- pars[,1]
alpha <- pars[,5]
tau <- pars[,2:4]

#--- data simulation function for GPCM
sim_gpcm_irt_param <- function(alpha, beta, tau, N, mu=0, sigma=1)
{
    theta <- stats::rnorm(N, mean=mu, sd=sigma)
    I <- length(beta)
    K <- ncol(tau)
    dat <- matrix(0, nrow=N, ncol=I)
    colnames(dat) <- paste0("I",1:I)
    for (ii in 1:I){
        probs <- matrix(0, nrow=N, ncol=K+1)
        for (kk in 1:K){
            probs[,kk+1] <- probs[,kk] + alpha[ii]*( theta - beta[ii] - tau[ii,kk] )
        }
        probs <- exp(probs)
        probs <- probs/rowSums(probs)
        rn <- stats::runif(N)
        cumprobs <- t(apply(probs,1,cumsum))
        for (kk in 1:K){
            dat[,ii] <- dat[,ii] + ( rn > cumprobs[,kk] )
        }
    }
    return(dat)
}

#-- simulate data
N <- 20000     # number of persons
set.seed(98)
dat1 <- sim_gpcm_irt_param(alpha=alpha, beta=beta, tau=tau, N=N, mu=0, sigma=1)
head(dat1)

#* generate design matrix for IRT parameterization
A1 <- TAM::.A.PCM2( resp=dat1)

#- estimate GPCM model
mod1 <- TAM::tam.mml.2pl( resp=dat1, irtmodel="GPCM", A=A1)
summary(mod1)

# compare true and estimated slope estimates (alpha)
cbind( alpha, mod1$B[,2,] )

# compare true and estimated item difficulties (beta)
cbind( beta, mod1$xsi$xsi[1:5] / mod1$B[,2,1] )

# compare true and estimated tau parameters
cbind( tau[,-3], matrix( mod1$xsi$xsi[-c(1:5)], nrow=5, byrow=TRUE ) / mod1$B[,2,1] )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Marginal maximum likelihood estimation (MML)}
%% \keyword{Multidimensional item response model}
%% \keyword{summary}
