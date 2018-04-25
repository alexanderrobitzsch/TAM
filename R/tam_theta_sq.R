## File Name: tam_theta_sq.R
## File Version: 0.03

tam_theta_sq <- function(theta, is_matrix = FALSE ){
	D <- ncol(theta)
	TP <- nrow(theta)
	theta2 <- tam_rcpp_theta_sq( theta=theta )
    theta2 <- array(theta2 , dim=c(TP,D,D) )		
	if (is_matrix){
		theta2 <- matrix( theta2 , nrow= TP , ncol= D^2 ) 
	}
    return(theta2)
}   
  
theta.sq2 <- tam_theta_sq
