## File Name: tam_np_create_spline_basis_bsplines.R
## File Version: 0.06


tam_np_create_spline_basis_bsplines <- function(nodes, n_basis)
{
    require_namespace_msg(pkg="splines")
    theta <- nodes
    des <- data.frame(theta=nodes)
    NK <- n_basis
    if (NK < 4){
        stop("Choose value of 'n_basis' larger than 3!\n")
    }
    NK <- NK - 3
    knots <- seq( min(theta), max(theta), len=NK+2 )
    knots <- knots[ 1 + 1:NK ]
    desm <- stats::model.matrix( ~ theta + splines::bs(theta, degree=2,
                            knots=knots, intercept=TRUE), data=des)
    return(desm)
}
