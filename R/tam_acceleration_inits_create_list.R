## File Name: tam_acceleration_inits_create_list.R
## File Version: 0.02

tam_acceleration_inits_create_list <- function(acceleration, parm, w=.35,
        w_max = .95, beta_new=0, beta_old=0)
{
    xsi_acceleration <- list( "acceleration" = acceleration , "w" = w ,
                            "w_max" = w_max ,
                            parm_history = cbind( parm, parm, parm ) ,
                            "beta_new" = beta_new ,
                            "beta_old" = beta_old
                                )
    return(xsi_acceleration)
}
