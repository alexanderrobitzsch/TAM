## File Name: tam_linking_joint.R
## File Version: 0.088

tam_linking_joint <- function(NM, parameters_list, linking_args, verbose=TRUE)
{
    wgt <- linking_args$wgt
    theta <- linking_args$theta
    type <- linking_args$type
    pow_rob_hae <- linking_args$pow_rob_hae
    eps_rob_hae <- linking_args$eps_rob_hae
    fix.slope <- linking_args$fix.slope
    par_init <- linking_args$par_init

    #- control arguments
    control <- list()
    if (verbose){
        control <- list(trace=2)
    }

    combis <- t(utils::combn(x=1L:NM, m=2))
    NP <- nrow(combis)
    joint_items <- list()
    joint_items_indices <- list()
    N_joint_items <- rep(0,NP)
    K <- ncol(parameters_list[[1]]$AXsi)
    N_common <- as.data.frame(matrix(0, nrow=NM, ncol=NM))
    rownames(N_common) <- colnames(N_common) <- paste0('study',1L:NM)
    for (pp in 1L:NP){
        parm1 <- parameters_list[[ combis[pp,1] ]]
        parm2 <- parameters_list[[ combis[pp,2] ]]
        joint_items_pp <- intersect(parm1$linking_items, parm2$linking_items)
        joint_items[[pp]] <- joint_items_pp
        joint_items_indices_pp <- list()
        joint_items_indices_pp[[1]] <- match( joint_items_pp, parm1$items)
        joint_items_indices_pp[[2]] <- match( joint_items_pp, parm2$items)
        joint_items_indices[[pp]] <- joint_items_indices_pp
        N_joint_items[pp] <- length(joint_items[[pp]])
        N_common[combis[pp,1], combis[pp,2]] <- N_joint_items[pp]
        N_common[combis[pp,2], combis[pp,1]] <- N_joint_items[pp]
    }

    #--- define linking function
    par <- c( rep(0,NM-1), rep(1, NM-1) )
    if (!is.null(par_init)){
        par <- par_init
    }
    linking_criterion_multiple_studies <- function(x){
        bvec <- c(0, x[1L:(NM-1)])
        avec <- c(1, x[NM-1 + 1L:(NM-1)])
        # transform parameters
        probs_list <- as.list(1L:NM)
        for (mm in 1L:NM){
            parameters_list_mm <- parameters_list[[mm]]
            probs_list[[mm]] <- tam_linking_joint_calc_probs(a=avec[mm], b=bvec[mm],
                                    parameters_list_mm=parameters_list_mm, theta=theta)
        }
        crit <- 0
        for (pp in 1L:NP){
            if (N_joint_items[pp] > 0 ){
                probs_pp1 <- probs_list[[ combis[pp,1] ]]
                probs_pp2 <- probs_list[[ combis[pp,2] ]]
                joint_items_indices_pp <- joint_items_indices[[pp]]
                probs_pp1 <- probs_pp1[,joint_items_indices_pp[[1]],,drop=FALSE ]
                probs_pp2 <- probs_pp2[,joint_items_indices_pp[[2]],,drop=FALSE ]
                crit_pp <- tam_linking_irf_discrepancy(probs1=probs_pp1, probs2=probs_pp2,
                                wgt=wgt, type=type, pow_rob_hae=pow_rob_hae,
                                eps_rob_hae=eps_rob_hae)
                crit <- crit + crit_pp
            }
        }
        return(crit)
    }

    lower <- rep(-Inf,2*(NM-1))
    upper <- rep(Inf,2*(NM-1))
    if (fix.slope){
        eps <- 1E-15
        lower[NM-1+c(1,NM-1)] <- 1 - eps
        upper[NM-1+c(1,NM-1)] <- 1 + eps
    }
    optim_result <- stats::optim( par=par, fn=linking_criterion_multiple_studies,
                        method='L-BFGS', lower=lower, upper=upper,
                        control=control)
    par_optim <- optim_result$par

    #--- transformation item parameters
    trafo_items <- matrix( 0, nrow=NM, ncol=2)
    rownames(trafo_items) <- paste0( 'study',1L:NM)
    colnames(trafo_items) <- c('a','b')
    trafo_items <- as.data.frame(trafo_items)
    trafo_items$a <- c(1,par_optim[NM-1+1L:(NM-1)])
    trafo_items$b <- c(0,par_optim[1L:(NM-1)])

    #- transformation person parameters
    trafo_persons <- 1 / trafo_items
    trafo_persons['b'] <- - trafo_items['b'] / trafo_items['a']

    #- means and standard deviations
    M_SD <- data.frame(M=rep(0,NM), SD=0, d=0)
    rownames(M_SD) <- rownames(trafo_persons)
    for (mm in 1L:NM){
        M_SD$M[mm] <- parameters_list[[mm]]$M
        M_SD$SD[mm] <- parameters_list[[mm]]$SD
        M_SD$SD[mm] <- M_SD$SD[mm]*trafo_persons[mm,'a']
        M_SD$M[mm] <- M_SD$M[mm]*trafo_persons[mm,'a'] + trafo_persons[mm,'b']
    }
    M_SD$d <- M_SD$M / mean(M_SD$SD)

    #- transformed item parameter
    for (mm in 1L:NM){
        parm_mm <- parameters_list[[mm]]
        trafo_items_mm <- c(a=trafo_items[mm,'a'], b=trafo_items[mm,'b'])
        res <- tam_linking_transform_item_parameters( B=parm_mm$B, AXsi=parm_mm$AXsi,
                        A=parm_mm$A, trafo_items=trafo_items_mm )
        parm_mm <- tam_linking_include_list( list1=parm_mm, list2=res )
        parameters_list[[mm]] <- parm_mm
    }

    #--- output
    res <- list(M_SD=M_SD, N_common=N_common, trafo_persons=trafo_persons,
                trafo_items=trafo_items, parameters_list=parameters_list,
                par_optim=par_optim)
    return(res)
}
