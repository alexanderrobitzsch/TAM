## File Name: tam_summary_print_ic.R
## File Version: 0.274

tam_summary_print_ic <- function( object, digits_ic=0, digits_values=2, bayes_crit=FALSE )
{
    ic <- object$ic
    calc_ic <- object$calc_ic
    if ( is.null(calc_ic) ){
        calc_ic <- TRUE
    }
    if ( calc_ic ){
        #-- extract available criteria
        crits <- c("AIC", "AIC3", "AICc", "BIC", "aBIC", "CAIC", "GHP")
        crits <- intersect( names(ic), crits )
        #-- print all criteria
        for (crit in crits){
            res <- tam_summary_print_ic_one_ic(ic=ic, crit=crit, digits_ic=digits_ic,
                        digits_penalty=digits_values)
        }
        cat("\n")
        if (bayes_crit){
            #--- information criteria based on Bayesian inference
            cat("Criteria based on Fully Bayesian Inference\n")
            cat( "\nDbar", "=", round( object$ic$Dbar, digits_values )  )
            cat( "\nDhat", "=", round( object$ic$Dhat, digits_values )  )
            cat( "\npD", "=", round( object$ic$pD, digits_values )  )
            cat( "\nDIC", "=", round( object$ic$DIC, digits_ic )," | penalty=",
                            round( 2*object$ic$pD, digits_values ) )
            cat("   | DIC=Dhat + 2*pD\n\n" )
        }
    }
}
