
tam_mml_person_EAP_rel <- function(EAP, SD.EAP , pweights=NULL)
{
	if (is.null(pweights)){
		pweights <- rep(1, length(EAP) )
	}
	EAP.variance <- weighted_mean( EAP^2 , pweights ) - ( weighted_mean( EAP , pweights ) )^2
	EAP.error <- weighted_mean( SD.EAP^2 , pweights )
	EAP.rel <- EAP.variance / ( EAP.variance + EAP.error )	
	return(EAP.rel)
}