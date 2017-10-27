## File Name: tam_theta_sq1.R
## File Version: 0.01


#############################################################
# rewrite theta.sq function into Rcpp
tam_theta_sq1 <- function(theta)
{
    theta2 <- array(,dim = c(nrow(theta), ncol(theta) , ncol(theta) ) )
    for( qq in 1:nrow(theta) ){
		theta2[qq,,] <- tcrossprod( theta[qq,] )  		
	}
    return(theta2 = theta2)
} 
#############################################################

theta.sq <- tam_theta_sq1
