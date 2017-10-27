## File Name: tam_print_call.R
## File Version: 0.01


#************************************************
# print CALL in summary										
tam_print_call <- function(CALL){					
	cat("\n\nCall:\n", paste(deparse(CALL), sep = "\n", collapse = "\n"), 
				"\n\n", sep = "")	
}
#************************************************							
