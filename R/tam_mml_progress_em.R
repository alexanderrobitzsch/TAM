## File Name: tam_mml_progress_em.R
## File Version: 0.17

tam_mml_progress_em <- function(progress, deviance, deviance_change, iter,
        rel_deviance_change, xsi_change, beta_change, variance_change, B_change,
        is_latreg=FALSE, is_mml_3pl=FALSE, guess_change=0,
        skillspace="normal", delta_change=0, digits_pars=6, devch,
        penalty_xsi=0, is_np=FALSE, np_change=NULL )
{
    if (progress){
        disp_fct <- "Deviance"
        if ( penalty_xsi !=0 ){
            disp_fct <- "Log posterior"
        }
        #----- display deviance
        cat( paste( "\n ", disp_fct, "=", round( deviance, 4 ) ))
        if (iter > 1){
            cat( " | Absolute change:", round( devch, 4 ) )
            cat( " | Relative change:", round( rel_deviance_change, 8 ) )
            if ( devch < 0 & iter > 1 ){
                cat("\n!!! Deviance increases!                                        !!!!")
                cat("\n!!! Choose maybe fac.oldxsi > 0 and/or increment.factor > 1    !!!!")
            }
        }
        #--- display item parameters
        if ( ! is_latreg ){
            cat( "\n  Maximum item intercept parameter change:", round( xsi_change, digits_pars ) )
            cat( "\n  Maximum item slope parameter change:", round( B_change, digits_pars ) )
        }
        if ( is_mml_3pl ){
            cat( "\n  Maximum item guessing parameter change:", round( guess_change, digits_pars ) )
        }
        if ( is_np ){
            cat( "\n  Maximum item parameter change:", round( np_change, digits_pars ) )
        }
        #--- display distribution parameters
        if ( skillspace=="normal"){
            cat( "\n  Maximum regression parameter change:", round( beta_change, digits_pars ) )
            cat( "\n  Maximum variance parameter change:", round( variance_change, digits_pars ) )
        }
        if ( skillspace !="normal" ){
            cat( "\n  Maximum delta parameter change:", round( delta_change, digits_pars ) )
        }
        cat( "\n" )
        utils::flush.console()
    }
}
