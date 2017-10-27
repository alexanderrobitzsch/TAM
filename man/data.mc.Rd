%% File Name: data.mc.Rd
%% File Version: 2.07

\name{data.mc}
\alias{data.mc}
\docType{data}
\title{
Dataset with Raw and Scored Responses from Multiple Choice Items
}
\description{
Dataset of responses from multiple choice items, containing 143 students
on 30 items.
}
\usage{
data(data.mc)
}
\format{
  The dataset is a list with two elements. The entry \code{raw}
  contains unscored (raw) item responses and the entry \code{scored}
  contains the scored (recoded) item responses. The format is:
  
\code{List of 2} \cr
\code{ $ raw   : chr [1:143, 1:30] "A" "A" "A" "A" ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : NULL} \cr
\code{  .. ..$ : chr [1:30] "I01" "I02" "I03" "I04" ...} \cr
\code{ $ scored:'data.frame':} \cr
\code{  ..$ I01: num [1:143] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ I02: num [1:143] 1 1 1 0 1 1 1 1 1 1 ...} \cr
\code{  ..$ I03: num [1:143] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  [...]} \cr
\code{  ..$ I29: num [1:143] NA 0 1 0 1 0 0 0 0 0 ...} \cr
\code{  ..$ I30: num [1:143] NA NA 1 1 1 1 0 1 1 0 ...} \cr
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%%}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
%% \examples{
%%data(data.mc)
%%## maybe str(data.mc) ; plot(data.mc) ...
%%}
\keyword{datasets}
