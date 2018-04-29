## File Name: add.lead.R
## File Version: 0.04

add.lead <- function(x, width=max(nchar(x))){
    sprintf(paste('%0', width, 'i', sep=''), x)
}
