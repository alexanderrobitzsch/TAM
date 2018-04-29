## File Name: tam.se.R
## File Version: 9.07
tam.se <- function( tamobj , item_pars=TRUE, ...)
{
    SE.quick <- TRUE
    if(SE.quick){
        if(class(tamobj) == "tam.mml"){
            res <- tam_mml_se_quick( tamobj=tamobj, item_pars=item_pars, ...)
        }
        if(class(tamobj) == "tam.jml"){
            # res <- tam.jml.se( tamobj, ...)
        }
    } else {
            ## add SE.quick=FALSE
    }
    return(res)
}



