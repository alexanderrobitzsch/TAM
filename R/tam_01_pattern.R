## File Name: tam_01_pattern.R
## File Version: 0.02

###################################################################
# Function for defining different 0/1 pattern
tam_01_pattern <- function(x)
{
	x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    mdp <- (x %*% (2^((1:ncol(x)) - 1))) + 1
    misspattern <- mdp[,1]
    misspattern <- list( "miss.pattern" = mdp[,1] , 
                "mp.index" = match( mdp[,1] , sort( unique(mdp[,1] ) ) ) )
    return( misspattern )
}

resp.pattern3 <- tam_01_pattern
