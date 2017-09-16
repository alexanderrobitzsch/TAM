## File Name: summary.tam.mml.3pl.R
## File Version: 9.09
## File Last Change: 2017-09-16 13:43:03
#*******************************************************
# Summary for tam.mml.3pl object                 *
summary.tam.mml.3pl <- function( object , file = NULL , ...){

	tam_osink( file = file)

	cat("------------------------------------------------------------\n")
	
	#- package and R session
    tam_print_package_rsession(pack="TAM")			
	#- computation time
	tam_print_computation_time(object=object)
	
    cat("Multidimensional Item Response Model in TAM \n\n")
	irtmodel <- object$irtmodel
	cat("IRT Model" , irtmodel , " (Function 'tam.mml.3pl')")
    tam_print_call(object$CALL)	
	
	cat("------------------------------------------------------------\n")
	cat( "Number of iterations =" , object$iter , "\n\n" )
	
	ctr <- object$control
	cat("Skill space:" , ifelse(object$skillspace=="normal" ,
			"Normal Distribution", "Discrete Distribution" ) , "\n")
	
	if (object$skillspace=="normal"){	
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
							}
	cat( "\nDeviance = " , round( object$deviance , 2 ) , " | " )
    cat( "Log Likelihood = " , round( -object$deviance/2 , 2 ) , "\n" )	
    cat( "Number of persons = " , object$nstud , "\n" )    
    cat( "Number of persons used = " , object$ic$n , "\n" ) 
	
	if( ! is.null( object$formulaA)  ){	
	    cat( "Number of generalized items = " , object$nitems , "\n" )    
	    cat( "Number of items = " , ncol(object$resp_orig) , "\n" )  		
								} else {
	    cat( "Number of items = " , object$nitems , "\n" )    				
						}
		
    cat( "Number of estimated parameters = " , object$ic$Npars , "\n" )    
    cat( "    Item threshold parameters  = " , object$ic$Nparsxsi , "\n" )    
    cat( "    Item slope parameters      = " , object$ic$NparsB , "\n" )    
    cat( "      Non-active item slopes   = " , 
							object$ic$Ngamma.nonactive , "\n" )    
	cat( "    Item guessing parameters   = " , object$ic$Nguess , "\n" )
    cat( "    Regression parameters      = " , object$ic$Nparsbeta , "\n" )    	
    cat( "    (Co)Variance parameters    = " , object$ic$Nparscov , "\n" )    		
    cat( "    Delta parameters           = " , object$ic$Ndelta , "\n\n" )    	
	
	#--- print information criteria
	res <- tam_summary_print_ic( object=object )	 
		
	#***********************
	# summary distribution: normal distribution
	PK <- ncol( object$theta)
	G <- object$G
	
	if (object$skillspace == "normal"){	
		cat("------------------------------------------------------------\n")
			cat("EAP Reliability\n")
			obji <- round( object$EAP.rel , 3 )
			print( obji )		
		cat("------------------------------------------------------------\n")
			cat("Covariances and Variances\n")
			if ( object$G >1){
				group_names <- paste0("Group" , object$groups )	
			}
			obji <- round( object$variance , 3 )
			if ( object$G >1){
				names(obji) <- group_names
				for (gg in 1:G){
				    var_gg <- object$variance[gg,,,drop=FALSE]
					ndim <- dim(var_gg)[3]
					var_gg <- matrix( var_gg , nrow=ndim , ncol=ndim )				
					obji <- round( var_gg , 3)	
					cat(group_names[gg],"\n")
					print(obji)
				}
			}			
			if (G==1){			
				print( obji )
			}
		cat("------------------------------------------------------------\n")
			cat("Correlations and Standard Deviations (in the diagonal)\n")
			if ( object$G > 1){
				# obji <- object$variance
				group_names <- paste0("Group" , object$groups )	
				for (gg in 1:G){
				    var_gg <- object$variance[gg,,,drop=FALSE]
					ndim <- dim(var_gg)[3]
					var_gg <- matrix( var_gg , nrow=ndim , ncol=ndim )				
					obji <- stats::cov2cor( var_gg )
					diag(obji) <- sqrt( diag( var_gg ) )
					obji <- round( obji , 3)	
					cat(group_names[gg],"\n")
					print(obji)
				}
			} else {
				obji <- stats::cov2cor(object$variance)
				diag(obji) <- sqrt( diag( object$variance) )
			}

			if (object$G == 1){
				obji <- round( obji, 3 )
				print( obji )
			}
		cat("------------------------------------------------------------\n")
		 cat("Regression Coefficients\n")
			obji <- round( object$beta , 5 )
			print( obji )		
				}   # end distribution skillspace=="normal"
    #*******************************************************************
    if (object$skillspace!="normal"){	
		cat("------------------------------------------------------------\n")
			cat("Trait distribution parameters delta\n")
			obji <- round( object$delta , 4 )
			colnames(obji) <- paste0("Group" , 1:object$G)
			print( obji )		
			TP <- nrow(obji)
	
	
	
if ( PK < 10 ){	
		cat("------------------------------------------------------------\n")
			cat("Full Trait distribution\n")
			obji <- round( object$pi.k , 4 )			
			colnames(obji) <- paste0("Group" , 1:object$G)
			if ( TP < 100 ){
				print( obji )				
							}
    cat("------------------------------------------------------------\n")
	cat("Moments of Trait Distribution\n")
	obji <- object$pi.k
	cat( "\nM Trait:\n" ) 
	print( round( t(object$moments$mean.trait ), 3 ) ) 
	cat( "\nSD Trait:\n" ) 
	print( round( t(object$moments$sd.trait ), 3 ) ) 
	cat( "\nSkewness Trait:\n" ) 
	print( round( t(object$moments$skewness.trait ), 3 ) ) 	
					cat( "\nCorrelations Trait: \n" )
					for (gg in 1:object$G){
						cat("Group" , gg , "\n")
						print( round( object$moments$correlation.trait[[gg]] , 3 ) )
								}
						}
					}
if (PK < 10 ){ 			
	cat("------------------------------------------------------------\n")		
		cat("Item Parameters -A*Xsi\n")
#		cat("   Item difficulties -A*Xsi are displayed in 'AXsi_'! \n\n")
		obji <- object$item
		for (vv in seq(2,ncol(obji) ) ){ obji[,vv] <- round( obji[,vv] , 3) }
		print(obji)
	# print xsi parameters if 
	if( ! is.null( object$formulaA)  ){
		cat("\nItem Facet Parameters Xsi\n")
#		cat("   Item difficulties -A*Xsi are displayed in 'AXsi_'! \n\n")
		obji <- object$xsi.facets
		for (vv in seq(3,ncol(obji) ) ){ obji[,vv] <- round( obji[,vv] , 3) }
		print(obji)
					}				
	if (( object$maxK > 2 ) | ( object$printxsi) ){
		cat("\nItem Parameters Xsi\n")
#		cat("   Item difficulties -A*Xsi are displayed in 'AXsi_'! \n\n")
		obji <- object$xsi
#		obji[,1] <- obji[,-1]
		for (vv in seq(1,ncol(obji) ) ){ obji[,vv] <- round( obji[,vv] , 3) }
		print(obji)
			}
		} else {  # PK >= 10
	cat("------------------------------------------------------------\n")		
	if (( object$maxK > 2 ) | ( object$printxsi) ){
		cat("\nItem Parameters Xsi\n")
#		cat("   Item difficulties -A*Xsi are displayed in 'AXsi_'! \n\n")
		obji <- object$xsi
#		obji[,1] <- obji[,-1]
		for (vv in seq(1,ncol(obji) ) ){ obji[,vv] <- round( obji[,vv] , 3) }
		print(obji)
			}
		cat("\nGammaslope Parameters\n")
		obji <- object$gammaslope
		print(round(obji,3)  )   			
		cat("\nGuessing Parameters\n")
		obji <- object$item$guess
		names(obji) <- colnames(object$resp)
		print(round(obji,3)  )   					
		
		
		}
					
	#******
	tam_csink(file=file)	
}
#*******************************************************
