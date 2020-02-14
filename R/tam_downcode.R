## File Name: tam_downcode.R
## File Version: 0.03

tam_downcode <- function(dat)
{
    res <- NULL
    dat1 <- NA*dat
    I <- ncol(dat)
    
    for (ii in 1L:I){
        v1 <- dat[,ii]
        vals_ii <- sort(unique(v1))
        n_ii <- length(vals_ii)
        ind_ii <- match(v1, vals_ii)
        dat1[,ii] <- ind_ii-1
        rec_ii <- data.frame(item=colnames(dat)[ii], orig=vals_ii, rec=0:(n_ii-1) )
        rec <- rbind(rec, rec_ii)
    }
    res <- list(dat=dat1, rec=rec)
    return(res)
}
