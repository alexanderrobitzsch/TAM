## File Name: tam_print_call.R
## File Version: 0.12


#--- print CALL in summary
tam_print_call <- function(CALL)
{
    s3 <- paste0(CALL, collapse=" ")
    if ( nchar(s3) < 3000 ){
        cat("\nCall:\n", paste(deparse(CALL), sep="\n", collapse="\n"), "\n\n", sep="")
    }
}
