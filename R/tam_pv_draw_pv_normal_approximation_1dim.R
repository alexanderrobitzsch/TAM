## File Name: tam_pv_draw_pv_normal_approximation_1dim.R
## File Version: 0.07

tam_pv_draw_pv_normal_approximation_1dim <- function( theta, nstud, ntheta, pv, hwt, pp)
{
    thetaM <- matrix( theta[,1] , nstud , ntheta , byrow=TRUE )
    EAP <- rowSums( thetaM * hwt )
    SDPost <- sqrt( rowSums( thetaM^2 * hwt ) - EAP^2 )
    pv[,pp] <- theta1 <- stats::rnorm( nstud , mean = EAP , sd = SDPost )
    #--- OUTPUT
    res <- list( pv=pv, theta1 = theta1 )
    return(res)
}
