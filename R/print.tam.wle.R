
#############################################
# print method for tam.wle objects
print.tam.wle <- function(x, digits = 3 , ...)
{
	ndim <- attr(x,"ndim")
	nobs <- attr(x,"nobs")
    CALL <- attr(x,"call")
	WLE.rel <- attr(x,"WLE.rel")
	M_sq_error <- attr(x,"M_sq_error")
	WLEvar <- attr(x,"WLEvar")
	WLEM <- attr(x,"WLEM")
	#--- print general informations
	res <- tam_wle_print_general( ndim=ndim, nobs=nobs, CALL=CALL )		
    #--- print reliabilities	
    res <- tam_wle_print_WLErel( WLE.rel=WLE.rel, ndim=ndim, digits=digits,
				M_sq_error=M_sq_error, WLEvar=WLEvar, WLEM=WLEM) 
}
##############################################


