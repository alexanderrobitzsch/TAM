
#*******************************************************
# summary
summary.tam.pv.mcmc <- function( object , file = NULL , ...)
{

	CDM::osink( file = file , suffix = "__SUMMARY.Rout" )
						
	cat("------------------------------------------------------------\n")
	
	cat( tam_packageinfo("TAM") , "\n" )	
	cat( tam_rsessinfo() , "\n\n")				
	
	cat( "Date of Analysis:" , paste(object$time[2]) , "\n" )
	cat("Computation time:" , print(object$time[2] - object$time[1]), "\n\n")
	
    cat("Bayesian Estimation of Plausible Values")
	
	# print Call
    tam_print_call(object$CALL)	
	
	cat("------------------------------------------------------------\n")
	cat( "Number of iterations =" , object$n.iter , "\n" )
	cat( "Number of burnin iterations =" , object$n.burnin , "\n" )

	cat("------------------------------------------------------------\n")
	
	cat("\nCriteria based on Marginal Likelihood\n")
	
    cat( "\nDeviance = " , round( object$ic$deviance , 2 ) , " | " )
    cat( "Log Likelihood = " , round( -object$ic$deviance/2 , 2 ) , "\n" )	
    cat( "Number of persons = " , object$ic$n , "\n" )    			
    cat( "Number of estimated parameters = " , object$ic$Npars , "\n" )    
	
    cat( "AIC  =" , round( object$ic$AIC , 0 ) , " | penalty =" , round( object$ic$AIC - object$ic$deviance ,2 ) , 
			"   | AIC = -2*LL + 2*p  \n" )    
    cat( "AICc =" , round( object$ic$AICc , 0 ) ," | penalty =" , round( object$ic$AICc - object$ic$deviance ,2 ) )
		cat("    | AICc = -2*LL + 2*p + 2*p*(p+1)/(n-p-1)  (bias corrected AIC)\n" )   	
    cat( "BIC  =" , round( object$ic$BIC , 0 ) , " | penalty =" , round( object$ic$BIC - object$ic$deviance ,2 ) , 
			"   | BIC = -2*LL + log(n)*p  \n" )  
    cat( "aBIC =" , round( object$ic$aBIC , 0 ) , " | penalty =" , round( object$ic$aBIC - object$ic$deviance ,2 ) , 
			"   | aBIC = -2*LL + log((n-2)/24)*p  (adjusted BIC) \n" ) 
    cat( "CAIC =" , round( object$ic$CAIC , 0 ) ," | penalty =" , round( object$ic$CAIC - object$ic$deviance ,2 ) )
		cat("   | CAIC = -2*LL + [log(n)+1]*p  (consistent AIC)\n\n" )   

	cat("Criteria based on Fully Bayesian Inference\n")		
    cat( "\nDbar =" , round( object$ic$Dbar , 2 )  )
	cat( "\nDhat =" , round( object$ic$Dhat , 2 )  )
	cat( "\npD   =" , round( object$ic$pD , 2 )  )
    cat( "\nDIC  =" , round( object$ic$DIC , 0 ) ," | penalty =" , 
					round( 2*object$ic$pD ,2 ) )
		cat("   | DIC = Dhat + 2*pD\n\n" )
	
	#----- properties of plausible values
	cat("------------------------------------------------------------\n")
	cat("Plausible Values\n\n")

	cat( "Number of plausible values =" , attr(object$pv, "nplausible") , "\n" )
	cat( "Iterations between PVs =" , attr(object$pv, "pv_lag") , "\n" )	
	cat( "Average acceptance rate =" , 
				round( attr(object$theta_acceptance_MH, "M_accrate"),3) , "\n" )	
	cat( "Average MH adjustment factor =" , 
				round( attr(object$theta_acceptance_MH, "M_adj_MH"),3) , "\n" )	
		
	cat("\nAutocorrelation of Drawn Plausible Values\n")
	obji <- round( object$theta_acf , 3 )
	print( obji )	
	
	cat("\nEAP Reliability\n")
	obji <- round( object$EAP_rel , 3 )
	print( obji )			
	
	cat("------------------------------------------------------------\n")
		
	cat("Regression Parameters\n")
	obji <- object$parameter_summary
	for (vv in seq(2,ncol(obji) ) ){ 
		obji[,vv] <- round( obji[,vv] , 3) 
	}
	print(obji)

	#******
	CDM::csink(file)
	
}
#*******************************************************
