## File Name: tam.wle.R
## File Version: 9.13

tam.wle <- function( tamobj, ... )
{
	CALL <- match.call()
	if(class(tamobj) == "tam.mml"){
		res <- tam.mml.wle2( tamobj, ...)
	}
	if(class(tamobj) == "tamaan"){
		res <- tam.mml.wle2( tamobj, ...)
	}
	if(class(tamobj) == "tam.jml"){
		res <- tam.jml.WLE( tamobj, ...)
	}
	attr(res,"call") <- CALL
	return( res )
}

