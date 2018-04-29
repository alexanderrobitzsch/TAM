## File Name: tam_rowcumsums1.R
## File Version: 0.03


tam_rowcumsums1 <- function(m1)
{
    g1 <- 0*m1
    g1[,1] <- m1[,1]
    for (ss in seq(2,ncol(m1))){
        g1[,ss] <- g1[,ss-1] + m1[,ss]
    }
    return(g1)
}

rowcumsums <- tam_rowcumsums1
