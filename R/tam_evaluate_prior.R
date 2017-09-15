## File Name: tam_evaluate_prior.R
## File Version: 0.08
## File Last Change: 2017-06-19 13:28:49

tam_evaluate_prior <- function( prior_list , parameter , derivatives=TRUE, 
		h = 1E-4)
{
	#-- extract parameters from prior list
	NP <- length(parameter)
	d2 <- d1 <- d0 <- rep( 0 , NP )
	if ( ! is.null(prior_list) ){
		is_prior <- attr( prior_list , "is_prior" )
		prior_entries <- attr( prior_list , "prior_entries" )
		LPE <- attr( prior_list , "length_prior_entries" )
		#--- evaluate prior at parameter
		if (LPE>0){
			for (pp in 1:LPE){
				entry_pp <- prior_entries[pp]
				prior_pp <- prior_list[[ entry_pp ]]
				density_pp <- prior_pp[[1]]
				args_pp <- prior_pp[[2]]
				parameter_pp <- parameter[pp]
				d0[entry_pp] <- tam_prior_eval_log_density_one_parameter( density_pp=density_pp, 
										args_pp=args_pp , parameter_pp=parameter_pp )								
				if (derivatives){
					d1[entry_pp] <- tam_prior_eval_log_density_one_parameter( density_pp=density_pp, 
											args_pp=args_pp , parameter_pp=parameter_pp + h,
											deriv=1 )
					d2[entry_pp] <- tam_prior_eval_log_density_one_parameter( density_pp=density_pp, 
											args_pp=args_pp , parameter_pp=parameter_pp - h,
											deriv=2)
				}
			}
		}
	}
	#-- evaluate difference quotient
	# res <- tam_difference_quotient( d0=d0 , d0p=d0p , d0m=d0m , h=h)
	# d1 <- res$d1
	# d2 <- res$d2

	#--- OUTPUT
	res <- list(d0=d0, d1=d1, d2=d2)
	return(res)
}
