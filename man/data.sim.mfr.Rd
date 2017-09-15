%% File Name: data.sim.mfr.Rd
%% File Version: 2.11
%% File Last Change: 2017-06-22 17:41:51

\name{data.sim.mfr}

\alias{data.sim.mfr}
\alias{data.sim.facets}

\docType{data}

\title{
Simulated Multifaceted Data
}

\description{
Simulated data from multiple facets. 
}

%% ## DON'T FORGET TO SET SEEDS BEFORE SIMULATING ACTUAL SIM-DATA
%% ## (NOT DONE YET)
%% ## Safe simulation script in "inst"-folder


\usage{
 data(data.sim.mfr)
 data(data.sim.facets)
}

\format{
 The format of \code{data.sim.mfr} is: \cr 
\code{ num [1:100, 1:5] 3 2 1 1 0 1 0 1 0 0 ...} \cr
\code{ - attr(*, "dimnames")=List of 2} \cr
\code{  ..$ : chr [1:100] "V1" "V1.1" "V1.2" "V1.3" ...} \cr
\code{  ..$ : NULL} 

The format of \code{data.sim.facets} is: \cr
\code{'data.frame':   100 obs. of  3 variables:} \cr
\code{ $ rater : num  1 2 3 4 5 1 2 3 4 5 ...} \cr
\code{ $ topic : num  3 1 3 1 3 2 3 2 2 1 ...} \cr
\code{ $ female: num  2 2 1 2 1 1 2 1 2 1 ...} \cr
}

%\details{
 %% ...
% }

\source{
 Simulated
}

%\references{
 %% none
%}

\examples{
#######
# sim multi faceted Rasch model
data(data.sim.mfr)
data(data.sim.facets)

  # 1: A-matrix test_rater
  test_1_items <- TAM::.A.matrix( data.sim.mfr, formulaA = ~rater, 
            facets = data.sim.facets, constraint = "items" )
  test_1_cases <- TAM::.A.matrix( data.sim.mfr, formulaA = ~rater, 
            facets = data.sim.facets, constraint = "cases" )
  
  # 2: test_item+rater
  test_2_cases <- TAM::.A.matrix( data.sim.mfr, formulaA = ~item+rater, 
            facets = data.sim.facets, constraint = "cases" )
  
  # 3: test_item+rater+topic+ratertopic
  test_3_items <- TAM::.A.matrix( data.sim.mfr, formulaA = ~item+rater*topic, 
            facets = data.sim.facets, constraint = "items" )
  # conquest uses a different way of ordering the rows
  # these are the first few rows of the conquest design matrix
  # test_3_items$A[grep("item1([[:print:]])*topic1", rownames(test_3_items)),] 
  
  # 4: test_item+step
  test_4_cases <- TAM::.A.matrix( data.sim.mfr, formulaA = ~item+step, 
            facets = data.sim.facets, constraint = "cases" )

  # 5: test_item+item:step
  test_5_cases <- TAM::.A.matrix( data.sim.mfr, formulaA = ~item+item:step, 
            facets = data.sim.facets, constraint = "cases" )
  test_5_cases$A[, grep("item1", colnames(test_5_cases)) ]

  # 5+x: more
  #  => 6: is this even well defined in the conquest-design output
  #          (see test_item+topicstep_cases.cqc / .des)
  #        regardless of the meaning of such a formula; 
  #        currently .A.matrix throws a warning
  # test_6_cases <- TAM::.A.matrix( data.sim.mfr, formulaA = ~item+topic:step, 
  #                 facets = data.sim.facets, constraint = "cases" )
  test_7_cases <- TAM::.A.matrix( data.sim.mfr, formulaA = ~item+topic+topic:step, 
            facets = data.sim.facets, constraint = "cases" )

\dontrun{
  # => 8: same as with 6
  test_8_cases <- TAM::.A.matrix( data.sim.mfr, formulaA = ~item+rater+item:rater:step, 
            facets = data.sim.facets, constraint = "cases" )
## [1] "Can't proceed the estimation: Lower-order term is missing."            
  test_9_cases <- TAM::.A.matrix( data.sim.mfr, formulaA = ~item+step+rater+item:step+item:rater, 
            facets = data.sim.facets, constraint = "cases" )
  test_10_cases <- TAM::.A.matrix( data.sim.mfr, formulaA = ~item+female+item:female, 
            facets = data.sim.facets, constraint = "cases" )

  ### All Design matrices
  test_1_cases <- TAM::designMatrices.mfr( data.sim.mfr, formulaA = ~rater, 
            facets = data.sim.facets, constraint = "cases" )
  test_4_cases <- TAM::designMatrices.mfr( data.sim.mfr, formulaA = ~item+item:step, 
            facets = data.sim.facets, constraint = "cases" )
  
  ### TAM
  test_4_cases <- TAM::tam.mml.mfr( data.sim.mfr, formulaA = ~item+item:step )
  test_tam <- TAM::tam.mml( data.sim.mfr )

  test_1_cases <- TAM::tam.mml.mfr( data.sim.mfr, formulaA = ~rater, 
            facets = data.sim.facets, constraint = "cases" )
  test_2_cases <- TAM::tam.mml.mfr( data.sim.mfr, formulaA = ~item+rater, 
            facets = data.sim.facets, constraint = "cases" )}
}

\keyword{datasets}


