## File Name: tam_mml_2pl_sufficient_statistics_item_slope.R
## File Version: 0.02

tam_mml_2pl_sufficient_statistics_item_slope <- function(hwt, theta, cResp,
	pweights, maxK, nitems, ndim)
{
    thetabar <- hwt %*% theta
    cB_obs <- crossprod( cResp*pweights , thetabar)
    B_obs <- aperm(array(cB_obs,dim=c(maxK, nitems,ndim)),c(2,1,3))
	#--- output
	res <- list(thetabar=thetabar, cB_obs=cB_obs, B_obs = B_obs)
	return(res)
}
