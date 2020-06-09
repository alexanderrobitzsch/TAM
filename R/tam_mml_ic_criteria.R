## File Name: tam_mml_ic_criteria.R
## File Version: 0.06


tam_mml_ic_criteria <- function(ic)
{
    dev <- ic$deviance
    # AIC
    ic$AIC <- dev + 2*ic$np
    # AIC3
    ic$AIC3 <- dev + 3*ic$np
    # BIC
    ic$BIC <- dev + ( log(ic$n) )*ic$np
    # adjusted BIC
    ic$aBIC <- dev + ( log( ( ic$n -2 ) / 24 ) )*ic$np
    # CAIC (consistent AIC)
    ic$CAIC <- dev + ( log(ic$n) + 1 )*ic$np
    # corrected AIC
    ic$AICc <- ic$AIC + 2*ic$np * ( ic$np + 1 ) / ( ic$n - ic$np - 1 )
    # Gilula-Haberman log penalty
    if (!is.null(ic$ghp_obs)){
        ic$GHP <- ic$AIC / (2*ic$ghp_obs)
    }
    #--- OUTPUT
    return(ic)
}
