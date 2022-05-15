## File Name: tam.wle.R
## File Version: 9.171

tam.wle <- function( tamobj, ... )
{
    CALL <- match.call()
    if(inherits(tamobj,"tam.mml")){
        res <- tam.mml.wle2( tamobj, ...)
    }
    if(inherits(tamobj,"tamaan")){
        res <- tam.mml.wle2( tamobj, ...)
    }
    if(inherits(tamobj,"tam.jml")){
        res <- tam_jml_wle( tamobj, ...)
    }
    attr(res,"call") <- CALL
    return( res )
}

