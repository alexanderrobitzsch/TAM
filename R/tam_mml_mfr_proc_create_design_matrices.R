
tam_mml_mfr_proc_create_design_matrices <- function(pid, maxKi, resp, formulaA,
	facets, constraint, ndim, Q, A, B, progress, xsi.fixed, resp00, B00,
	beta.fixed)
{
	diffKi <- FALSE
	xsi.elim <- NULL
	var_ki <- stats::var( maxKi )
	if ( is.na(var_ki) ){ 
		var_ki <- 0 
	}
	
	if ( var_ki > 1E-3 ){ 
	    diffKi <- TRUE
		design <- designMatrices.mfr2(resp=resp, formulaA=formulaA, facets=facets,  
                        constraint=constraint, ndim=ndim,
                        Q=Q, A=A, B=B , progress=progress)
		xsi.elim <- design$xsi.elim
		if ( ! is.null(xsi.elim) ){	
			if ( nrow(xsi.elim) > 0 ){
				xsi.elim2 <- cbind( xsi.elim[,2] , 99 )		 
				xsi.fixed <- rbind( xsi.fixed , xsi.elim2 )
			}
		}
		# set first beta coefficient to zero
		if ( is.null( beta.fixed ) ){
			dimB <- dim(design$B$B.3d.0	)	
		    beta.fixed <- cbind( 1 , 1:dimB[3] , 0)
		}
	} else {				
         design <- designMatrices.mfr(resp, formulaA=formulaA, facets=facets,  
                        constraint=constraint, ndim=ndim,
                        Q=Q, A=A, B=B , progress=progress)									 
	}	 																																							
    A <- design$A$A.3d.0	
    cA <- design$A$A.flat.0	
    B <- design$B$B.3d.0
    Q <- design$Q$Q.flat.0
    X <- design$X$X
    X.red <- design$X$X.noStep
    gresp <- design$gresp$gresp
    gresp.noStep <- design$gresp$gresp.noStep
    xsi.constr <- design$xsi.constr

	#****************************
	items00 <- colnames(resp00)
	I00 <- length(items00)
	D <- dim(B00)[3]
	if ( ! is.null(B00) ){		
		rownames_A <- dimnames(A)[[1]]
		for (ii in 1:I00){
			ind <- grep( paste0( items00[ii] , "-" ) , rownames_A )
			if ( length(ind) > 0 ){
				I2 <- length(ind)
				for (vv in 1:I2){
					B[ ind[vv] , , 1:D] <- B00[ii , , 1:D ,drop=FALSE] * ( B[ ind[vv] , , 1:D] != 0  )
				}						
			}		
		}									
	}   # end is.null()	
    if ( is.null( pid ) ){ 
		pid <- 1:(nrow(gresp) ) 
	}    
    design <- NULL   
    if (progress){ 
		cat("    * Created Design Matrices   (", 
			paste(Sys.time()) , ")\n")
		utils::flush.console()	  
    }        
	#--- OUTPUT
	res <- list(pid=pid, diffKi=diffKi, var_ki=var_ki, xsi.fixed=xsi.fixed, xsi.elim=xsi.elim,
				beta.fixed=beta.fixed, A=A, cA=cA, B=B, Q=Q, X=X, X.red=X.red, gresp=gresp,
				gresp.noStep=gresp.noStep, xsi.constr=xsi.constr, design=design, D=D)
	return(res)
}
