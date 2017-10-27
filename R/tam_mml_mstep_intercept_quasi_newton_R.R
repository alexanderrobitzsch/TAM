## File Name: tam_mml_mstep_intercept_quasi_newton_R.R
## File Version: 0.06

tam_mml_mstep_intercept_quasi_newton_R <- function( rprobs, converge, Miter, Msteps,
	nitems, A, AXsi, B, xsi, theta, nnodes, maxK, est.xsi.index, itemwt, indexIP.no,
	indexIP.list2, Avector, ItemScore, xsi.fixed, eps=1E-20, old_increment , convM,
	fac.oldxsi, oldxsi, trim_increment, progress, np, increments_msteps )
{

	#--- begin algorithm
    while ( ! converge & ( Miter <= Msteps ) ) {	       
        if (Miter > 1){ 
			res.p <- tam_mml_calc_prob( iIndex=1:nitems , A=A , AXsi=AXsi , B=B , 
                                 xsi=xsi , theta=theta , nnodes=nnodes, maxK=maxK)					
			rprobs <- res.p$rprobs
        }

		res <- tam_calc_exp( rprobs=rprobs, A=A, np=np, est.xsi.index=est.xsi.index, 
					itemwt=itemwt, indexIP.no=indexIP.no, indexIP.list2=indexIP.list2, 
					Avector=Avector ) 						 
        xbar <- res$xbar
        xbar2 <- res$xbar2
        xxf <- res$xxf	        
		
        # Compute the difference between sufficient statistic and expectation
        diff <- as.vector(ItemScore) - xbar
        #Compute the Newton-Raphson derivative for the equation to be solved
        deriv <- xbar2 - xxf
		# res <- tam_evaluate_prior( prior_list = prior_list_xsi , parameter = xsi )		
		# diff <- diff + res$d1
		# deriv <- abs(deriv) + abs( res$d2 )
		
		#-- define increments
        increment <- diff*abs( 1/( deriv + eps ) )
        if ( ! is.null( xsi.fixed) ){ 
			increment[ xsi.fixed[,1] ] <- 0 
		} 
		#--- trim increments
		increment <- tam_trim_increment( increment=increment, max.increment=old_increment, 
							trim_increment=trim_increment)		
		old_increment <- increment        
        ##**SE
        se.xsi <- sqrt( 1 / abs(deriv) )
        if ( ! is.null( xsi.fixed) ){ 
			se.xsi[ xsi.fixed[,1] ] <- 0 
		} 
        ##**	        
        xsi <- xsi+increment

		max_change <- max(abs(increment))				
		increments_msteps[Miter] <- max_change		
        if ( max_change < convM ){ converge <- TRUE }

        Miter <- Miter + 1						        
        # stabilizing the algorithm
        if (fac.oldxsi > 0 ){
			xsi <-  (1-fac.oldxsi) * xsi + fac.oldxsi *oldxsi
        }			   
        # progress bar
        if (progress){ 
			cat("-")
			utils::flush.console()
        }		
    } # end of all parameters loop
	#--------------------------------------
	# output
	res <- list( xsi=xsi, Miter=Miter, increments_msteps=increments_msteps, 
					se.xsi=se.xsi, logprior_xsi = 0 )
	return(res)
}	
