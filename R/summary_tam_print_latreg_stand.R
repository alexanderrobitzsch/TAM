## File Name: summary_tam_print_latreg_stand.R
## File Version: 0.04

summary_tam_print_latreg_stand <- function(object, digits_stand=4)
{
    if ( ! is.null( object$latreg_stand ) ){
        cat("------------------------------------------------------------\n")
        cat("Standardized Coefficients\n")
        tam_round_data_frame_print(obji=object$latreg_stand$beta_stand,
                                        digits=digits_stand, from=3)
        cat("\n** Explained Variance R^2\n")
        tam_round_data_frame_print(obji=object$latreg_stand$R2_theta,
                                        digits=digits_stand)
        cat("** SD Theta\n")
        tam_round_data_frame_print(obji=object$latreg_stand$sd_theta,
                                        digits=digits_stand)
        cat("** SD Predictors\n")
        tam_round_data_frame_print(obji=object$latreg_stand$sd_x,
                                        digits=digits_stand)
    }
}
