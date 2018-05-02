## File Name: tam_mml_progress_proc_nodes.R
## File Version: 0.05

tam_mml_progress_proc_nodes <- function( progress, snodes, nnodes , maxnodes=8000,
        skillspace="normal", QMC=FALSE)
{
    if (skillspace != "normal"){
        progress <- FALSE
    }
    if (progress){
        l1 <- paste0( "    * ")
        if (snodes==0){
            l1 <- paste0(l1 , "Numerical integration with ")
        } else{
            if (QMC){
                l1 <- paste0(l1 , "Quasi Monte Carlo integration with ")
            } else {
                l1 <- paste0(l1 , "Monte Carlo integration with ")
            }
        }
        cat( paste0( l1 , nnodes , " nodes\n") )
        if (nnodes > maxnodes){
            cat("      @ Are you sure that you want so many nodes?\n")
            cat("      @ Maybe you want to use Quasi Monte Carlo integration with fewer nodes.\n")
        }
    }
}
