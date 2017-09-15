## File Name: add.colnames.resp.R
## File Version: 9.07
## File Last Change: 2017-06-18 12:08:59

add.colnames.resp <- function(resp){
  if( is.null(colnames(resp)) ){
    I <- ncol(resp)
    colnames(resp) <- paste0("I",1:I) 
  }
  return(resp)
}
