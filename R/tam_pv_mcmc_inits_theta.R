
tam_pv_mcmc_inits_theta <- function(person, theta_init)
{
	#--- extract EAPs as initial theta estimates
	cnp <- colnames(person)
	ind <- setdiff( grep("EAP" , cnp ) , grep("SD" , cnp ) )
	theta0 <- as.matrix(person[ , ind , drop=FALSE])
	D <- ncol(theta0)
	colnames(theta0) <- paste0("Dim", 1:D)
	if ( ! is.null(theta_init)){
		theta0 <- theta_init
	}
	return(theta0)
}