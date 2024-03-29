## File Name: tam.se.R
## File Version: 9.161

tam.se <- function( tamobj, item_pars=TRUE, ...)
{
    SE.quick <- TRUE
    #-------------------------------
    ## "quick" standard errors
    if(SE.quick){
        if(inherits(tamobj,"tam.mml") ){
            res <- tam_mml_se_quick( tamobj=tamobj, item_pars=item_pars, ...)
        }
        if(inherits(tamobj,"tam.latreg") ){
            res <- tam_latreg_se_quick( tamobj=tamobj, ...)
        }

        if(inherits(tamobj,"tam.jml")){
            # res <- tam.jml.se( tamobj, ...)
            ## include standard errors here!!
        }
    }
    #-------------------------------
    ## standard errors based on observed log likelihood
    if( ! SE.quick ){
            ## add SE.quick=FALSE
    }
    return(res)
}



