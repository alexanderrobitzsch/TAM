## File Name: tam_mml_3pl_stud_prior_discrete.R
## File Version: 0.01
## File Last Change: 2017-05-02 13:12:08

tam_mml_3pl_stud_prior_discrete <- function(pi.k, ntheta, G , 
	group1.list, gwt)
{
	for (gg in 1:G){
		ind.gg <- group1.list[[gg]]
		gwt[ind.gg,] <- matrix( pi.k[,gg] , nrow=length(ind.gg) , 
								ncol=ntheta , byrow=TRUE )						
    }
	res <- list(gwt = gwt)
	return(res)
}
