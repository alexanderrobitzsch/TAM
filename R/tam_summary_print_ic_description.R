## File Name: tam_summary_print_ic_description.R
## File Version: 0.02

tam_summary_print_ic_description <- function(crit)
{ 	
	#--- description for criteria
	crit_desc <- NULL
	if ( crit == "AIC"){	
		crit_desc <- "AIC = -2*LL + 2*p" 	
	}
	if ( crit == "AIC3"){	
		crit_desc <- "AIC3 = -2*LL + 3*p" 	
	}	
	if ( crit == "AICc"){	
		crit_desc <- "AICc = -2*LL + 2*p + 2*p*(p+1)/(n-p-1)  (bias corrected AIC)" 	
	}
	if ( crit == "BIC"){	
		crit_desc <- "BIC = -2*LL + log(n)*p" 	
	}
	if ( crit == "aBIC"){	
		crit_desc <- "aBIC = -2*LL + log((n-2)/24)*p  (adjusted BIC)" 	
	}
	if ( crit == "CAIC"){	
		crit_desc <- "CAIC = -2*LL + [log(n)+1]*p  (consistent AIC)" 	
	}
	#--- output
	return(crit_desc)	
}
