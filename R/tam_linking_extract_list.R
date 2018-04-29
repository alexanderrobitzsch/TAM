## File Name: tam_linking_extract_list.R
## File Version: 0.05

tam_linking_extract_list <- function( input, entries, names=NULL )
{
    if ( is.null(names) ){
        names <- entries
    }
    NE <- length(entries)
    output <- list()
    for (ee in 1:NE){
        output[[ names[ee] ]] <- input[[ entries[ee] ]]
    }
    return(output)
}
