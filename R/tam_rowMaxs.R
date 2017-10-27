## File Name: tam_rowMaxs.R
## File Version: 0.01

	
#############################################################
# search the maximum in each matrix row
tam_rowMaxs <- function(mat, na.rm = FALSE)
{    
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

rowMaxs <- tam_rowMaxs
