## File Name: tam_print_computation_time.R
## File Version: 0.01
## File Last Change: 2017-09-16 13:39:45

tam_print_computation_time <- function(object)
{
	cat( "Date of Analysis:" , paste(object$time[2]) , "\n" )
	cat("Computation time:" , print(object$time[2] - object$time[1]), "\n\n")
}
