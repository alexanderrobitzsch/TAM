## File Name: tam_pv_mcmc_inits_sampled_parameters_objects.R
## File Version: 0.11

tam_pv_mcmc_inits_sampled_parameters_objects <- function( n.burnin, n.iter,
    beta, variance, G, Y, beta_groups)
{
    #----------------------
    #--- beta
    NB <- length( as.vector( unlist(beta) ) )
    N_samples <- n.iter - n.burnin
    beta_samples <- matrix( NA, nrow=N_samples, ncol=NB)
    attr(beta_samples,"last_sample") <- 0
    D <- ncol(variance[[1]])
    v1 <- c()
    v3 <- NULL
    for (dd in 1:D){
        v1 <- c(v1, paste0( colnames(Y), ".Dim", dd ) )
    }
    v1 <- paste0("beta_", v1 )
    v2 <- v1
    if (beta_groups){
        v2 <- c()
        np <- NB / G
        for (gg in 1:G){
            v2 <- c(v2, paste0(v1, "_group",gg) )
            v3[[gg]] <- (gg-1)*np + 1:np
        }
    }
    attr(beta_samples,"parnames") <- v2
    attr(beta_samples,"beta_groups") <- beta_groups
    colnames(beta_samples) <- v2
    attr(beta_samples,"beta_index") <- v3
    attr(beta_samples,"D") <- D
    attr(beta_samples,"G") <- G
    attr(beta_samples,"ncol_Y") <- ncol(Y)

    #----------------------
    #--- variance
    ND <- ( D*(D+1) / 2 )
    NV <- ND  * G
    variance_samples <- matrix( NA, nrow=N_samples, ncol=NV)
    attr(variance_samples,"last_sample") <- 0
    v1 <- list()
    for (gg in 1:G){
        v1[[gg]] <- ( gg-1)*ND + 1:ND
    }
    attr(variance_samples,"variance_index") <- v1
    attr(variance_samples,"variance_select") <- ! upper.tri( variance[[gg]] )
    v0 <- NULL
    for (dd in 1:D){
        for (ee in dd:D){
            v0 <- c( v0, paste0("Sigma[", ee, ",", dd, "]") )
        }
    }
    v1 <- v0
    if (G>1){
        v1 <- NULL
        for (gg in 1:G){
            v1 <- c( v1, paste0( v0, "_group", gg ) )
        }
    }
    attr(variance_samples,"parnames") <- v1
    colnames(variance_samples) <- v1

    saved_iter <- seq( n.burnin + 1, n.iter)
    attr(beta_samples, "saved_iterations") <- saved_iter
    attr(variance_samples, "saved_iterations") <- saved_iter

    #--- deviance
    deviance_samples <- rep(NA, N_samples)
    attr(deviance_samples,"last_sample") <- 0

    #--- theta values
    nstud <- nrow(Y)
    theta_samples_mean <- matrix(0,nrow=nstud, ncol=D)
    theta_samples_sd <- theta_samples_mean

    #--- OUTPUT
    res <- list(beta_samples=beta_samples, variance_samples=variance_samples,
                    deviance_samples=deviance_samples, N_samples=N_samples,
                    saved_iter=saved_iter, theta_samples_mean=theta_samples_mean,
                    theta_samples_sd=theta_samples_sd )
    return(res)
}

