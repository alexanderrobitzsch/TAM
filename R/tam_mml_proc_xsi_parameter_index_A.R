## File Name: tam_mml_proc_xsi_parameter_index_A.R
## File Version: 0.06

tam_mml_proc_xsi_parameter_index_A <- function(A, np)
{
    # Create an index linking items and parameters
    indexIP <- colSums(aperm(A, c(2,1,3)) !=0, na.rm=TRUE)
    # define list of elements for item parameters
    indexIP.list <- list( 1:np )
    for ( kk in 1:np ){
        indexIP.list[[kk]] <- which( indexIP[,kk] > 0 )
    }
    lipl <- cumsum( sapply( indexIP.list, FUN=function(ll){ length(ll) } ) )
    indexIP.list2 <- unlist(indexIP.list)
    indexIP.no <- as.matrix( cbind( c(1, lipl[-length(lipl)]+1 ), lipl ) )
    #--- OUTPUT
    res <- list( indexIP=indexIP, indexIP.list=indexIP.list,
            indexIP.list2=indexIP.list2, indexIP.no=indexIP.no)
    return(res)
}
