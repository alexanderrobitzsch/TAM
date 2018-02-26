## File Name: summary.tam.R
## File Version: 9.38

#****** summary for tam object                              
summary.tam <- function( object , file = NULL , ...)
{
	tam_osink( file = file)
						
	latreg <- FALSE
	if ( class(object) == "tam.latreg" ){
		latreg <- TRUE 
		object$irtmodel <- "tam.latreg"
	}

	cat("------------------------------------------------------------\n")
	
	#--- package and R session
	tam_print_package_rsession(pack="TAM")			
	#--- computation time
	tam_print_computation_time(object=object)
	
    cat("Multidimensional Item Response Model in TAM \n\n")
	irtmodel <- object$irtmodel	
	cat("IRT Model" , irtmodel )
	
	#--- print call
    tam_print_call(object$CALL)	
	
	cat("------------------------------------------------------------\n")
	cat( "Number of iterations =" , object$iter , "\n" )
	
	ctr <- object$control
	if (ctr$snodes==0){
		cat("Numeric integration with" , dim(object$theta)[1] , "integration points\n")
	}
	if (ctr$snodes>0){
		if (ctr$QMC){
			cat("Quasi Monte Carlo integration with" , dim(object$theta)[1] , "integration points\n")
		}
		if (! ctr$QMC){
			cat("Monte Carlo integration with" , dim(object$theta)[1] , "integration points\n")
		}						
	}					
	
    cat( "\nDeviance = " , round( object$deviance , 2 ) , "\n" )
    cat( "   Log likelihood = " , round( object$ic$loglike , 2 ) , "\n" )	
	# cat( "   Log prior = " , round( object$ic$logprior , 2 ) , "\n" )	
	# cat( "   Log posterior = " , round( object$ic$logpost , 2 ) , "\n\n" )		    
	
	cat( "Number of persons = " , object$nstud , "\n" )    
    cat( "Number of persons used = " , object$ic$n , "\n" )  
	
	if( ! is.null( object$formulaA)  ){	
	    cat( "Number of generalized items = " , object$nitems , "\n" )    
	    cat( "Number of items = " , ncol(object$resp_orig) , "\n" )  		
	} else {
	    cat( "Number of items = " , object$nitems , "\n" )    				
	}
		
    cat( "Number of estimated parameters = " , object$ic$Npars , "\n" )    
	if (! latreg ){
		cat( "    Item threshold parameters  = " , object$ic$Nparsxsi , "\n" )    
		cat( "    Item slope parameters      = " , object$ic$NparsB , "\n" )    
	}
    cat( "    Regression parameters      = " , object$ic$Nparsbeta , "\n" )    	
    cat( "    (Co)Variance parameters    = " , object$ic$Nparscov , "\n\n" )    		
	
	#--- print information criteria
	res <- tam_summary_print_ic( object=object )

	cat("------------------------------------------------------------\n")
	cat("EAP Reliability\n")
	obji <- round( object$EAP.rel , 3 )
	print( obji )		
	cat("------------------------------------------------------------\n")
	cat("Covariances and Variances\n")
	if ( object$G >1){
		a1 <- stats::aggregate( object$variance , list( object$group ) , mean )
		object$variance <- a1[,2]
	}
	obji <- round( object$variance , 3 )
	if ( object$G >1){
		names(obji) <- paste0("Group" , object$groups )
	}		
	print( obji )
	cat("------------------------------------------------------------\n")
	cat("Correlations and Standard Deviations (in the diagonal)\n")
	if ( object$G >1){
		obji <- sqrt( object$variance )
	} else {
		obji <- stats::cov2cor(object$variance)
		diag(obji) <- sqrt( diag( object$variance) )
	}
	if ( object$G >1){
		names(obji) <- paste0("Group" , object$groups )			
	}		
	tam_round_data_frame_print(obji=obji, digits=3)		
				
	cat("------------------------------------------------------------\n")
    cat("Regression Coefficients\n")
	tam_round_data_frame_print(obji=object$beta, digits=5)		
	
	#--- print standardized regression coefficients
	summary_tam_print_latreg_stand(object=object, digits_stand=4)
	
	if ( ! latreg ){		
		cat("------------------------------------------------------------\n")		
		cat("Item Parameters -A*Xsi\n")
		obji <- object$item
		tam_round_data_frame_print(obji=obji, from=2, to=ncol(obji), digits=3, rownames_null=FALSE)	
		
		# print xsi parameters if 
		if( ! is.null( object$formulaA)  ){
			cat("\nItem Facet Parameters Xsi\n")
			obji <- object$xsi.facets
			xsi99 <- sum( object$xsi == 99 )
			if ( xsi99 > 0 ){
				cat("\nSome item xsi parameters are not estimable ")
				cat(" which is indicated by values of 99\n\n")	
			}
			if ( object$PSF ){
				cat("\nA pseudo facet 'psf' with zero effects with all zero effects\n")
				cat("was created because of non-unique person-facet combinations.\n\n") 							
			}														
			tam_round_data_frame_print(obji=obji, from=3, digits=3)						
		}				
		if (( object$maxK > 2 ) | ( object$printxsi) ){
			cat("\nItem Parameters Xsi\n")
			obji <- object$xsi
			tam_round_data_frame_print(obji=obji, from=1, digits=3)
		}
		#*******************
		# output efa
		if ( object$irtmodel %in% c("efa") ){
			cat("------------------------------------------------------------\n")	
			cat("\nStandardized Factor Loadings Oblimin Rotation\n")		
			print(object$efa.oblimin)
		}						
		#*******************
		# output bifactor models	
		if ( object$irtmodel %in% c("bifactor1" , "bifactor2","efa") ){
			cat("------------------------------------------------------------\n")	
			if (irtmodel=="efa"){
				cat("\nStandardized Factor Loadings (Schmid Leimann transformation)\n")		
				obji <- object$B.SL
			} else {
				cat("\nStandardized Factor Loadings (Bifactor Model)\n")		
				obji <- object$B.stand				
			}
			tam_round_data_frame_print(obji=obji, digits=3)	
			meas <- object$meas									
			cat("\nDimensionality/Reliability Statistics\n\n")	
			cat("ECV (Omega Asymptotical)=" , round( meas["ECV(omega_a)"] ,3 ) , "\n")
			cat("Omega Total =" , round( meas["omega_t"] ,3 ) , "\n")
			cat("Omega Hierarchical =" , round( meas["omega_h"] ,3 ) , "\n")		
			if (object$maxK==2){		
				cat("Omega Total (GY) =" , round( meas["omega_tot_diff"] ,3 ) , "\n")				
				cat( "  Omega Total GY (Green & Yang, 2009) includes item difficulties\n")
				cat( "  and estimates the reliability of the sum score.\n")	
			}
		}
	}
	#******
	tam_csink(file=file)
}
#*******************************************************

summary.tam.mml <- summary.tam
summary.tam.2pl <- summary.tam
summary.tam.mfr <- summary.tam
summary.tam.latreg <- summary.tam
