## File Name: tam.personfit.R
## File Version: 0.04

tam.personfit <- function(tamobj)
{
	#** WLE estimation in case of MML estimation
	if ( class(tamobj) != "tam.jml" ){
		res <- tam.wle(tamobj, progress=FALSE)
		tamobj$theta <- res$theta
		tamobj$xsi <- tamobj$xsi$xsi	
	}
	#** calculate fit statistics
	res <- tam.jml.fit( tamobj=tamobj )$fit.person
	return(res)
}
