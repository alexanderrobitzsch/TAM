## File Name: tam_summary_print_ic.R
## File Version: 0.06
## File Last Change: 2017-09-16 13:05:50

tam_summary_print_ic <- function( object )
{
	ic <- object$ic
	#-- extract available criteria
	crits <- c("AIC", "AIC3", "AICc" , "BIC", "aBIC", "CAIC")
	crits <- intersect( names(ic) , crits )
	#-- print all criteria
	for (crit in crits){
		res <- tam_summary_print_ic_one_ic(ic=ic, crit=crit)
	}
	cat("\n")
}
