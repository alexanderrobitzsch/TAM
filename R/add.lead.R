## File Name: add.lead.R
## File Version: 0.02
## File Last Change: 2017-09-15 17:53:28

add.lead <- function(x, width=max(nchar(x))){
  sprintf(paste('%0', width, 'i', sep=''), x) 
}
