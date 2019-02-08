## File Name: tam_np_group_lasso_update.R
## File Version: 0.288

tam_np_group_lasso_update <- function(par_old, grad, hess_max, lambda,
    penalty_type="lasso", n_ii=1, eps=1e-4)
{
    #* lasso-type penalties
    is_lasso_type <- penalty_type %in% c("lasso","scad","mcp")
    is_ridge <- penalty_type=="ridge"
    #* computations
    hess_max <- hess_max*(1 + eps)
    par_new <- par_old - grad / hess_max
    par_lam <- hess_max * par_new
    n_par <- length(par_lam)
    wgt_lambda <- sqrt(n_par)
    lambda_temp <- lambda * wgt_lambda
    par_lam_norm <- tam_group_lasso_norm(x=par_lam)
    regularized <- FALSE
    if (is_lasso_type){
        par1 <- CDM::cdm_parameter_regularization(x=par_lam_norm,
                            regular_type=penalty_type, regular_lam=lambda_temp)
        if (par1==0){
            regularized <- TRUE
        }
        par_reg <- par1 * par_lam / par_lam_norm / hess_max
    }
    if (is_ridge){
        par_reg <- par_lam / (hess_max + 2*lambda)
    }
    par_reg_norm <- tam_group_lasso_norm(x=par_reg)*wgt_lambda
    n_reg <- n_par * regularized
    if (is_lasso_type){
        pen <- CDM::cdm_penalty_values(x=par_reg_norm, regular_type=penalty_type,
                    regular_lam=lambda)
        par_reg_penalty <- n_ii*pen
    }
    if (is_ridge){
        par_reg_penalty <- n_ii*lambda*sum(par_reg^2)
    }

    #--- output
    res <- list(par_reg=par_reg, par_reg_norm=par_reg_norm, n_par=n_par,
                    n_reg=n_reg, regularized=regularized,
                    penalty_type=penalty_type, par_reg_penalty=par_reg_penalty)
    return(res)
}
