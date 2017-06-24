
tam_mml_se_AXsi <- function(AXsi, A, se.xsi, maxK)
{
    se.AXsi <- 0*AXsi
    A1 <- A
    A1[ is.na(A) ] <- 0
    if ( length( se.xsi) > 1){
      se.xsiD <- diag( se.xsi^2 )
    } else {
      se.xsiD <- matrix( se.xsi^2,1,1)
    }		
    for (kk in 1:maxK){  # kk <- 1
	  dim_A1 <- dim(A1)
      A1_kk <- A1[,kk,]
      if ( is.vector(A1_kk) ){
        A1_kk <- matrix( A1_kk , nrow=dim_A1[1] , ncol=dim_A1[3] )
      }
      se.AXsi[,kk] <- sqrt( diag( A1_kk %*% se.xsiD %*% t( A1_kk ) ) )	
    }
	return(se.AXsi)
}