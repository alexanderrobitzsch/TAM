## File Name: summary.tam.modelfit.R
## File Version: 9.06

########################################################
# summary tam.modelfit method

summary.tam.modelfit <- function( object, ... )
{
    #*****
    cat("Test of Global Model Fit (Maximum Chi Square)\n")
    # cat("p=", round( object$modelfit.test$p.holm, 5 ) )
    print( round( object$modelfit.test, 5 ) )

    #******
    cat("\nMADaQ3 Statistic and Test of Global Model Fit (Maximum aQ3)\n")
    obji <- round( object$stat.MADaQ3, 4)
    print(obji)

    #****
    cat("\nSummary of Q3 and adjusted Q3 statistics (based on posterior distribution)\n")
    obji <- object$Q3_summary
    tam_round_data_frame_print(obji=obji, from=2, digits=4)

    #*****
    cat("\nFit Statistics\n")
    obji <- object$fitstat
    print( round(obji,3))
}
##############################################################

