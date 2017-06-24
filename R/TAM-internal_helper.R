
####################################################################
rowcumsums <-
  function(m1){
    g1 <- 0*m1
    g1[,1] <- m1[,1]
    for (ss in seq(2,ncol(m1))){
      g1[,ss] <- g1[,ss-1] + m1[,ss] 
    }
    return(g1)
  }

			
#############################################################
# search the maximum in each matrix row
rowMaxs <-
  function(mat, na.rm = FALSE){    
    # Call: from designMatrix()
    # Input: 
    # mat: numeric matrix
    # na.rm: logical. Should missing values (including NaN) be omitted from the calculations?
    # Output: row maxima of input matrix    
    n <- nrow(mat)
    p <- ncol(mat)
    x <- as.vector(mat)
    x <- matrix(x[order(rep(1:n, p), x, na.last = !na.rm)], p, n)
    x[p , ]
}

#############################################################
# rewrite theta.sq function into Rcpp
theta.sq <-
  function(theta){
    theta2 <- array(,dim = c(nrow(theta), ncol(theta) , ncol(theta) ) )
    for( qq in 1:nrow(theta) ){
		theta2[qq,,] <- tcrossprod( theta[qq,] )  		
				}
    return("theta2" = theta2)
}

  
#############################################################

add.lead <- function(x, width=max(nchar(x))){
  sprintf(paste('%0', width, 'i', sep=''), x) 
}

##############################################################
