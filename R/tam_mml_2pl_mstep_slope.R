## File Name: tam_mml_2pl_mstep_slope.R
## File Version: 9.44
## File Last Change: 2017-04-29 15:32:06

############################################################################

tam_mml_2pl_mstep_slope <- function (B_orig, B, B_obs, B.fixed , max.increment, nitems, A, 
            AXsi, xsi, theta, nnodes, maxK, itemwt, Msteps, ndim, convM ,
            irtmodel , progress , est.slopegroups , E , basispar , se.B ,
			equal.categ, B_acceleration, trim_increment="half" , iter , eps=1E-10)
{     
    old_increment <- array( max.increment , dim= c(nitems , maxK , ndim) )
    xbar2 <- xxf <- xbar <- array(0, dim=c( nitems , maxK ) )
    converge <- FALSE
    Biter <- 1
    mK <- 1:maxK		
    items.temp <- 1:nitems
    items.conv <- NULL
	increments_msteps <- rep(NA,Msteps)
	B_old <- B
	eps <- 1E-10
	NI <- nitems
	TP <- nrow(theta)
	if (progress){ 
		cat("\nM Step Slopes       |")
		utils::flush.console() 
	}	
	
    if (irtmodel == "GPCM"){
		old_increment.temp <- matrix( .3 , nrow=NI , ncol=ndim )	
    }
    if (irtmodel == "GPCM.design" ){
		Nlambda <- ncol(E)	# number of lambda parameters							
		old_increment.temp <- matrix( .3 , nrow=Nlambda , ncol=ndim )				
    }	
    if (irtmodel == "2PL.groups"){
		ES <- length( unique( est.slopegroups) )
		old_increment.temp <- array( .3 , dim=c(ES , maxK ,ndim )	)
    }	
	#------------------------------------------------	
	# begin algorithm M-steps
    while (!converge & ( Biter <= Msteps ) ) {  
		#compute expectation
		res.p <- tam_mml_calc_prob( iIndex=items.temp, A=A, AXsi=AXsi, B=B, xsi=xsi, 
					theta=theta, nnodes=nnodes, maxK=maxK ) 
		rprobs <- res.p[["rprobs"]] 
		LIT <- length(items.temp)
		######     D I M E N S I O N S     ######
		for (dd in 1:ndim){	
			if ( irtmodel %in% c("GPCM","GPCM.design") ){ 
				xtemp <- matrix(0 , nrow=LIT , ncol=TP )
			}
			if ( irtmodel == "2PL.groups"){ 
				xtemp <- array(0 , dim=c( LIT , TP , maxK) )
			}									
			##### C A T E G O R I E S #########	
			if ( ! equal.categ ){ 
				rprobs[ is.na(rprobs) ] <- 0 
			}
			for ( k in 1:maxK ){	
				rprobs.k <- matrix( rprobs[,k,] , nrow=LIT , ncol=TP )
				rpr.it <- t( rprobs.k ) * itemwt[,items.temp]
				ttheta.dd2 <- t( theta[,dd,drop=FALSE]^2)
				ttheta.dd <- t( theta[,dd,drop=FALSE] )
				xbar[items.temp,k] <- ttheta.dd %*% rpr.it
				xxf[items.temp,k] <- ttheta.dd2 %*% rpr.it
				if ( irtmodel == "2PL" ){ 	# begin usual 2PL
					xbar2[items.temp,k] <- ttheta.dd2 %*% ( t( rprobs.k^2 ) * itemwt[,items.temp] )
				}
				if ( irtmodel %in% c("GPCM","GPCM.design") ){ 	# begin GPCM
					xxf[items.temp,k] <- xxf[items.temp,k] * (k-1)^2	
					xtemp <- xtemp + tam_matrix2(theta[,dd],nrow=LIT,ncol=TP) * rprobs[,k,] * ( k-1 )	
				}
				if ( irtmodel == "2PL.groups"){ 	# begin 2PL groups
					xbar2[items.temp,k] <- 
						t( theta[,dd]^2 ) %*% t( rprobs[,k,]^2*t(itemwt[,items.temp]) )
						# t(A) %*% t( B * t(C) ) -> simplify this formula!!!
				}          
				if ( ! is.null( items.conv) ){
					xbar[ items.conv , ] <- 0
					xxf[ items.conv , ] <- 0
					xbar2[ items.conv , ] <- 0
				}		  
			}	# end categories k
 	
			#----------------
			if ( irtmodel %in% c("GPCM","GPCM.design")){  # begin GPCM / GPCM.design
				B_obs.temp <- B_obs
				B_obs.temp[ items.temp,,dd] <- 
					tam_matrix2( mK-1 , LIT , maxK ) * B_obs[ items.temp ,,dd]
				xbar[ is.na(xbar) ] <- 0		
				xbar.temp <- tam_matrix2( mK-1, LIT , maxK ) *xbar					
				diff.temp <- rowSums(B_obs.temp[,,dd]) - rowSums(xbar.temp)	
				xbar2.temp <- diag( xtemp^2 %*% itemwt[ , items.temp  ] )
				xxf.temp <- rowSums( xxf )		
				if ( ! is.null(items.conv) ){ 
					diff.temp[ items.conv, ] <- 0   
				}  
				deriv.temp <- xbar2.temp - xxf.temp
 		    }	# loop GPCM and GPCM.design
			#----------------
			if (irtmodel == "GPCM"){	  # begin GPCM
				deriv.temp[ is.na(deriv.temp)] <- eps	
				increment.temp <- diff.temp*abs(1/( deriv.temp + eps ) )
				increment.temp <- tam_trim_increment(increment=increment.temp, 
										max.increment=abs(old_increment.temp[,dd]), 
										trim_increment=trim_increment)									
				old_increment.temp[,dd] <- increment.temp	
				increment <- tam_outer( increment.temp , mK - 1) 
				if (Biter==1){ 
					se.B[,,dd]  <- tam_outer( sqrt( 1 / abs( deriv.temp )) , mK-1 ) 
				}
			}  # end GPCM
			#----------------	
			if ( irtmodel == "GPCM.design"){	  		
				diff.temp <- ( t(E) %*% diff.temp )[,1]
				deriv.temp <- ( t(E^2) %*% deriv.temp )[,1]
				increment.temp <- diff.temp*abs(1/( deriv.temp + eps ) )  
				increment.temp <- tam_trim_increment(increment=increment.temp, 
										max.increment=abs(old_increment.temp[,dd]), 
										trim_increment=trim_increment)
				old_increment.temp[,dd] <- increment.temp				
				basispar[,dd] <- basispar[,dd] + increment.temp
				increment.temp <- ( E %*% increment.temp )[,1]
				increment <- tam_outer( increment.temp , mK-1) 
				d1 <- tam_outer(  1 / abs( deriv.temp ) , mK-1 ) 
				LL <- ncol(d1)		
				for (ll in 1:LL){
					m1 <- sqrt( diag( E %*% d1[,ll] %*% t( d1[,ll] ) %*% t(E) ) )
					if (Biter==1){ 
						se.B[,ll,dd]  <- m1 
					}	  
				}
				nB <- dim(B)		
				B_ind <- 1 * ( B_orig != 0 )
				for (dd in 1:nB[3]){
					EB <- E %*% basispar[,dd] 
					for (cc in 1:(nB[2]-1)){ 
						B[,cc+1,dd] <- cc * EB * B_ind[,cc+1,dd]
					}
				}
				B00 <- B
			} # end GPCM.design																			        
			#----------------
			if (irtmodel == "2PL.groups"){  # begin 2PL slopegroups
				a1 <- stats::aggregate( B_obs[,,dd] - xbar  , list(est.slopegroups) , sum )		
				a2 <- stats::aggregate( xbar2 - xxf  , list(est.slopegroups) , sum )				
				deriv.temp <- as.matrix(a2[,-1])
				diff.temp <- as.matrix(a1[,-1])
				increment.temp <- diff.temp*abs(1/( deriv.temp + eps ) )  								
				increment.temp <- tam_trim_increment(increment=increment.temp, 
										max.increment=abs(old_increment.temp[,,dd]), 
										trim_increment=trim_increment)																		
				old_increment.temp[,,dd] <- increment.temp	
				ind.match <- match( est.slopegroups , a1[,1] )
				increment <- increment.temp[ ind.match , ]
				if (Biter==1){ se.B[ ,,dd]  <- sqrt( 1 / abs( deriv.temp[ind.match , ] )) }	  
			} # end 2PL slope groups
			#----------------
			if (irtmodel == "2PL"){		# begin 2PL					
				diff <- B_obs[,,dd] - xbar	
				if ( ! is.null( items.conv) ){  
					diff[ items.conv, ] <- 0	
				}
				deriv <- xbar2 - xxf	  
				increment <- diff*abs(1/( deriv + eps ) )        		  		  
				increment <- tam_trim_increment(increment=increment, 
										max.increment=abs(old_increment[,,dd]), 
										trim_increment=trim_increment)																		
				if (Biter==1){ 
					se.B[,,dd] <- sqrt( 1 / abs( deriv )) 
				}
			}   # end 2PL
			#----------------
			increment[B_orig[,,dd]==0] <- 0  # B[i,k,] could be zero for some dimensions
			old_increment[,,dd] <- increment        	
			B[,,dd] <- B[,,dd] + increment   # update B parameter
			if (irtmodel=="GPCM.design"){
				B <- B00
			}				
		}  # end dimensions dd
		#----------------------------
		if ( irtmodel == "2PL" ){
			items.temp <- which( apply( old_increment , 1 , 
                                    FUN = function(ll){ ! ( max(abs(ll)) < convM ) } ) )
			items.conv <- setdiff( 1:nitems , items.temp )	
		}
		max_change <- max(abs(old_increment))
		increments_msteps[Biter] <- max_change
		if ( max_change < convM ) { 
			converge <- TRUE 
		}
		Biter <- Biter + 1
		if (progress){ 
			cat( "-" ) ; utils::flush.console()	
		}		  
    }		# end while loop
	#**** end algorithm M-steps
	#---------------------------------------------
	se.B[ B_orig == 0 ] <- 0
	
	# acceleration
	if ( B_acceleration$acceleration != "none" ){		
		B_acceleration <- tam_accelerate_parameters( xsi_acceleration=B_acceleration , 
							xsi=as.vector(B) , iter=iter , itermin=3)							
		B <- array( B_acceleration$parm , dim(B) ) 
	}
	# change in B parameters	
	B_change <- max( abs( B- B_old) )	
	#---- OUTPUT
    res <- list( "B" = B , "basispar" = basispar , "se.B" = se.B, Biter=Biter,
				B_acceleration = B_acceleration, B_change = B_change , 
				increments_msteps = increments_msteps)
    return(res) 
}
#----------------------------------------------------------------------
  
Mstep_slope.v2 <- tam_mml_2pl_mstep_slope  
