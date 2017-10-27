## File Name: tam_pv_recommend_tam_pv_mcmc.R
## File Version: 0.01

tam_pv_recommend_tam_pv_mcmc <- function(tamobj)
{
	control <- tamobj$control
	if ( ! is.null(control) ){
		if ( control$snodes > 0 ){
			v1 <- paste0(" It is recommended to use TAM::tam.pv.mcmc in case\n",
						 " of fitted TAM models with stochastic nodes.\n")
			warning(v1, call. = FALSE )			
		}	
	}
}
