## File Name: tam_stud_prior_multiple_groups.R
## File Version: 0.10


tam_stud_prior_multiple_groups <- function(theta, Y, beta, variance, nstud,
            nnodes, ndim, YSD, unidim_simplify, G, group_indices, snodes=0,
            normalize=FALSE )
{
    if (G==1){
        gwt <- tam_stud_prior(theta=theta, Y=Y, beta=beta,
                    variance=variance, nstud=nstud,
                    nnodes=nnodes, ndim=ndim, YSD=YSD, unidim_simplify=unidim_simplify,
                    snodes=snodes, normalize=normalize )
    }
    if (G>1){
        gwt <- matrix( NA, nrow=nstud, ncol=nnodes)
        for (gg in 1:G){
            ind_gg <- group_indices[[gg]]
            nstud_gg <- length(ind_gg)
            variance_gg <- matrix( variance[gg,,], nrow=ndim, ncol=ndim)
            gwt_gg <- tam_stud_prior(theta=theta, Y=Y[ ind_gg,, drop=FALSE],
                    beta=beta, variance=variance_gg, nstud=nstud_gg,
                    nnodes=nnodes, ndim=ndim, YSD=YSD, unidim_simplify=unidim_simplify,
                    snodes=snodes, normalize=normalize )
            gwt[ ind_gg, ] <- gwt_gg
        }
    }
    return(gwt)
}
