%% File Name: TAM-utilities.Rd
%% File Version: 0.54

\name{TAM-utilities}
\alias{TAM-utilities}
\alias{tam_packageinfo}
\alias{tam_rsessinfo}
\alias{tam_print_call}
\alias{tam_args_CALL_search}
\alias{tam_matrix2}
\alias{tam_outer}
\alias{tam_normalize_matrix_rows}
\alias{tam_trim_increment}
\alias{tam_difference_quotient}
\alias{tam_assign_list_elements}
\alias{tam_aggregate}
\alias{tam_interval_index}
\alias{tam_rowCumsums}
\alias{tam_normalize_vector}
\alias{tam_remove_missings}
\alias{tam_AXsi_compute}
\alias{tam_AXsi_fit}
\alias{require_namespace_msg}
\alias{tam_dmvnorm}
\alias{add.lead}
\alias{tam_bayesian_bootstrap}
\alias{tam_cov_wt}
\alias{tam_cor_wt}
\alias{tam_round_data_frame}
\alias{tam_round_data_frame_print}
\alias{tam_osink}
\alias{tam_csink}
\alias{tam_ginv}


\title{Utility Functions in \pkg{TAM}}

\description{
Utility functions in \pkg{TAM}.
}

\usage{
## information about used package version
tam_packageinfo(pack)
## call statement in a string format
tam_print_call(CALL)
## information about R session
tam_rsessinfo()
## grep list of arguments for a specific variable
tam_args_CALL_search(args_CALL, variable, default_value)
## requireNamespace with message of needed installation
require_namespace_msg(pkg)
## add leading zeroes
add.lead(x, width=max(nchar(x)))
## round some columns in a data frame
tam_round_data_frame(obji, from=1, to=ncol(obji), digits=3, rownames_null=FALSE)
## round some columns in a data frame and print this data frame
tam_round_data_frame_print(obji, from=1, to=ncol(obji), digits=3, rownames_null=FALSE)
## copy of CDM::osink
tam_osink(file, suffix="__SUMMARY.Rout")
## copy of CDM::csink
tam_csink(file)

## base::matrix function with argument value byrow=TRUE
tam_matrix2(x, nrow=NULL, ncol=NULL)
## more efficient base::outer functions for operations "*", "+" and "-"
tam_outer(x, y, op="*")
## row normalization of a matrix
tam_normalize_matrix_rows(x)
## row normalization of a vector
tam_normalize_vector(x)
## aggregate function for mean and sum based on base::rowsum
tam_aggregate(x, group, mean=FALSE, na.rm=TRUE)
## column index when a value in a matrix is exceeded (used in TAM::tam.pv)
tam_interval_index(matr, rn)
## cumulative sum of row entries in a matrix
tam_rowCumsums(matr)
## extension of mvtnorm::dmvnorm to matrix entries of mean
tam_dmvnorm(x, mean, sigma, log = FALSE )
## Bayesian bootstrap in TAM (used in tam.pv.mcmc)
tam_bayesian_bootstrap(N, sample_integers=FALSE, do_boot=TRUE)
## weighted covariance matrix
tam_cov_wt(x, wt=NULL, method="ML")
## weighted correlation matrix
tam_cor_wt(x, wt=NULL, method="ML")
## generalized inverse
tam_ginv(x, eps=.05)

## remove items or persons with complete missing entries
tam_remove_missings( dat, items, elim_items=TRUE, elim_persons=TRUE )
## compute AXsi given A and xsi
tam_AXsi_compute(A, xsi)
## fit xsi given A and AXsi
tam_AXsi_fit(A, AXsi)

## trimming increments in iterations
tam_trim_increment(increment, max.increment, trim_increment="cut",
     trim_incr_factor = 2, eps = 1E-10, avoid_na = FALSE)
## numerical differentiation by central difference
tam_difference_quotient(d0, d0p, d0m, h)
## assign elements of a list in an environment
tam_assign_list_elements(x, envir)
}

\arguments{
\item{pack}{An \R package}
\item{CALL}{An \R call}
\item{args_CALL}{Arguments obtained from \code{as.list( sys.call() )}}
\item{variable}{Name of a variable}
\item{default_value}{Default value of a variable}
\item{pkg}{String}
\item{x}{Vector or matrix or list}
\item{width}{Number of zeroes before decimal}
\item{obji}{Data frame or vector}
\item{from}{Integer}
\item{to}{Integer}
\item{digits}{Integer}
\item{rownames_null}{Logical}
\item{file}{File name}
\item{suffix}{Suffix for file name of summary output}
\item{nrow}{Number of rows}
\item{ncol}{Number of columns}
\item{y}{Vector}
\item{op}{An operation \code{"*"}, \code{"+"} or \code{"-"}}
\item{group}{Vector of grouping identifiers}
\item{mean}{Logical indicating whether mean should be calculated or the sum or
vector or matrix}
\item{na.rm}{Logical indicating whether missing values should be removed}
\item{matr}{Matrix}
\item{sigma}{Matrix}
\item{log}{Logical}
\item{N}{Integer}
\item{sample_integers}{Logical indicating whether weights for complete cases
should be sampled in bootstrap}
\item{do_boot}{Logical}
\item{wt}{Optional vector containing weights}
\item{method}{Method, see \code{\link[stats:cov.wt]{stats::cov.wt}} }
\item{rn}{Vector}
\item{dat}{Data frame}
\item{items}{Vector}
\item{elim_items}{Logical}
\item{elim_persons}{Logical}
\item{A}{Array}
\item{xsi}{Vector}
\item{AXsi}{Matrix}
\item{increment}{Vector}
\item{max.increment}{Numeric}
\item{trim_increment}{One of the methods \code{"half"} or \code{"cut"}}
\item{trim_incr_factor}{Factor of trimming in method \code{"half"}}
\item{eps}{Small number preventing from division by zero}
\item{avoid_na}{Logical indicating whether missing values should be set to zero.}
\item{d0}{Vector}
\item{d0p}{Vector}
\item{d0m}{Vector}
\item{h}{Vector}
\item{envir}{Environment variable}
}

%\details{
%xxx
%%}


% \examples{
% #############################################################################
% }
